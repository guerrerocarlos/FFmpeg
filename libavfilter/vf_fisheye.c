/*
 * Copyright (c) 2010 Brandon Mintern
 * Copyright (c) 2007 Bobby Bingham
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * video fade filter
 * based heavily on vf_negate.c by Bobby Bingham
 */

#include "libavutil/avassert.h"
#include "libavutil/avstring.h"
#include "libavutil/common.h"
#include "libavutil/eval.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "drawutils.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

#define R 0
#define G 1
#define B 2
#define A 3

#define Y 0
#define U 1
#define V 2

#define FADE_IN  0
#define FADE_OUT 1

#define PI 3.1415926536

typedef struct FadeContext {
    const AVClass *class;
    int type;
    int depth;
    int factor, fade_per_frame;
    int start_frame, nb_frames;
    int hsub, vsub, bpp;
    unsigned int black_level, black_level_scaled;
    uint8_t is_packed_rgb;
    uint8_t rgba_map[4];
    int alpha;
    uint64_t start_time, duration;
    enum {VF_FADE_WAITING=0, VF_FADE_FADING, VF_FADE_DONE} fade_state;
    uint8_t color_rgba[4];  ///< fade color
    int black_fade;         ///< if color_rgba is black
} FadeContext;

typedef struct ThreadPayload {
    AVFrame *sphere;
    AVFrame *square;
    int w;
    int h;
    int hsub;
    int vsub;
    int cw;
    int ch;
    int bpp;
    int planes;
};

static av_cold int init(AVFilterContext *ctx)
{

    av_log(NULL, AV_LOG_INFO, "\n Initializing fisheye");


    FadeContext *s = ctx->priv;

    s->fade_per_frame = (1 << 16) / s->nb_frames;
    s->fade_state = VF_FADE_WAITING;

    if (s->duration != 0) {
        // If duration (seconds) is non-zero, assume that we are not fading based on frames
        s->nb_frames = 0; // Mostly to clean up logging
    }

    // Choose what to log. If both time-based and frame-based options, both lines will be in the log
    if (s->start_frame || s->nb_frames) {
        av_log(ctx, AV_LOG_VERBOSE,
               "type:%s start_frame:%d nb_frames:%d alpha:%d\n",
               s->type == FADE_IN ? "in" : "out", s->start_frame,
               s->nb_frames,s->alpha);
    }
    if (s->start_time || s->duration) {
        av_log(ctx, AV_LOG_VERBOSE,
               "type:%s start_time:%f duration:%f alpha:%d\n",
               s->type == FADE_IN ? "in" : "out", (s->start_time / (double)AV_TIME_BASE),
               (s->duration / (double)AV_TIME_BASE),s->alpha);
    }

    s->black_fade = !memcmp(s->color_rgba, "\x00\x00\x00\xff", 4);
    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV444P,      AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV420P,      AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV410P,      AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_NONE
    };

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

const static enum AVPixelFormat studio_level_pix_fmts[] = {
    AV_PIX_FMT_YUV444P,  AV_PIX_FMT_YUV422P,  AV_PIX_FMT_YUV420P,
    AV_PIX_FMT_YUV411P,  AV_PIX_FMT_YUV410P,
    AV_PIX_FMT_YUV440P,
    AV_PIX_FMT_NONE
};

static int config_props(AVFilterLink *inlink)
{
    // printf('config_props');

    FadeContext *s = inlink->dst->priv;
    const AVPixFmtDescriptor *pixdesc = av_pix_fmt_desc_get(inlink->format);

    s->hsub = pixdesc->log2_chroma_w;
    s->vsub = pixdesc->log2_chroma_h;

    s->depth = pixdesc->comp[0].depth;
    av_log(NULL, AV_LOG_INFO, "\nconfig_props depth: %d", s->depth);

    // s->do_slice = s->depth <= 8 ? vibrance_slice8 : vibrance_slice16;

    s->bpp = pixdesc->flags & AV_PIX_FMT_FLAG_PLANAR ?
             1 :
             av_get_bits_per_pixel(pixdesc) >> 3;
    s->alpha &= !!(pixdesc->flags & AV_PIX_FMT_FLAG_ALPHA);
    s->is_packed_rgb = ff_fill_rgba_map(s->rgba_map, inlink->format) >= 0;

    av_log(NULL, AV_LOG_INFO, "\ns->hsub: %d, s->vsub %d, s->bpp %d, s->alpha %d, s->is_packed_rgb %d", s->hsub, s->vsub, s->bpp, s->alpha, s->is_packed_rgb);


    /* use CCIR601/709 black level for studio-level pixel non-alpha components */
    s->black_level =
            ff_fmt_is_in(inlink->format, studio_level_pix_fmts) && !s->alpha ? 16 : 0;
    /* 32768 = 1 << 15, it is an integer representation
     * of 0.5 and is for rounding. */
    s->black_level_scaled = (s->black_level << 16) + 32768;
    return 0;
}

static av_always_inline void filter_rgb(FadeContext *s, const AVFrame *frame,
                                        int slice_start, int slice_end,
                                        int do_alpha, int step)
{
    int i, j;
    // printf('filter_rgb');
    const uint8_t r_idx  = s->rgba_map[R];
    const uint8_t g_idx  = s->rgba_map[G];
    const uint8_t b_idx  = s->rgba_map[B];
    const uint8_t a_idx  = s->rgba_map[A];
    const uint8_t *c = s->color_rgba;

    for (i = slice_start; i < slice_end; i++) {
        uint8_t *p = frame->data[0] + i * frame->linesize[0];
        for (j = 0; j < frame->width; j++) {
            // printf('av_clip_uint8');
#define INTERP(c_name, c_idx) av_clip_uint8(((c[c_idx]<<16) + ((int)p[c_name] - (int)c[c_idx]) * s->factor + (1<<15)) >> 16)
            p[r_idx] = INTERP(r_idx, 0);
            p[g_idx] = INTERP(g_idx, 1);
            p[b_idx] = INTERP(b_idx, 2);
            if (do_alpha)
                p[a_idx] = INTERP(a_idx, 3);
            p += step;
        }
    }
}

static int filter_slice_rgb(AVFilterContext *ctx, void *arg, int jobnr,
                            int nb_jobs)
{
    av_log(NULL, AV_LOG_INFO, "\n filter_slice_rgb, nb_jobs: %d", nb_jobs);

    FadeContext *s = ctx->priv;
    AVFrame *frame = arg;
    int slice_start = (frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (frame->height * (jobnr+1)) / nb_jobs;

    if      (s->alpha)    filter_rgb(s, frame, slice_start, slice_end, 1, 4);
    else if (s->bpp == 3) filter_rgb(s, frame, slice_start, slice_end, 0, 3);
    else if (s->bpp == 4) filter_rgb(s, frame, slice_start, slice_end, 0, 4);
    else                  av_assert0(0);

    return 0;
}

static int filter_slice_sphere(AVFilterContext *ctx, void *arg, int jobnr,
                             int nb_jobs)
{

    FadeContext *s = ctx->priv;
    AVFrame *frame = arg;
    int slice_start = (frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (frame->height * (jobnr+1)) / nb_jobs;
    int i, j;

    int plane = s->is_packed_rgb ? 0 : A;

    av_log(NULL, AV_LOG_INFO, "\n %d of %d [filter_slice_sphere] start %d end %d. -- plane: %d -- bpp: %d -- frame->linesize[0]: %d -- frame->width: %d", jobnr, nb_jobs, slice_start, slice_end, plane, s->bpp, frame->linesize[0], frame->width);
    
    for (i = slice_start; i < (slice_end); i++) {
        uint8_t *pr = frame->data[3] + i * frame->linesize[0];
        // uint8_t *pr = frame->data[1] + i * frame->linesize[0];
        // uint8_t *pb = frame->data[2] + i * frame->linesize[0];
        for (j = 0; j < frame->width * s->bpp; j++) {
            // for (plane = 0; plane < 4; plane++) {
                /* s->factor is using 16 lower-order bits for decimal
                * places. 32768 = 1 << 15, it is an integer representation
                * of 0.5 and is for rounding. */
                // *p = *p+10;
                // *(p+1) = *p-10;
                // *(p) = *(frame->data[0] + i * frame->linesize[0] - j);

                // if(i > (frame->height /2)) {
                //     *(pr) = *(pr + (i-30) * frame->linesize[0] + j);
                //     *(pr+1) = *(pr + (i-30) * frame->linesize[0] + j + 1);
                // } else {
                if(j>frame->width/2) {
                    *(pr+j) = *(pr+j-frame->width/2);
                } else {
                    *(pr+j) = *(pr+j);
                }
                // }
                // pr+=2;
                // p+=3;
            // }
        }
    }

    return 0;
}

static int filter_slice_luma(AVFilterContext *ctx, void *arg, int jobnr,
                             int nb_jobs)
{
    FadeContext *s = ctx->priv;
    AVFrame *frame = arg;
    int slice_start = (frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (frame->height * (jobnr+1)) / nb_jobs;
    int i, j;
    int c = 0;
    av_log(NULL, AV_LOG_INFO, "\n %d of %d [filter_slice_luma] start %d end %d, width %d, height %d",jobnr, nb_jobs, slice_start, slice_end, frame->width, frame->height);

    for (i = slice_start; i < slice_end; i++) {
        uint8_t *p = frame->data[0] + i * frame->linesize[0];
        for (j = 0; j < frame->width * s->bpp; j++) {
            /* s->factor is using 16 lower-order bits for decimal
             * places. 32768 = 1 << 15, it is an integer representation
             * of 0.5 and is for rounding. */
                // if(j>frame->width/2) {
                    // *(p+j) = *(p+j-frame->width/2);
                // } else {
                    *(p+j) = c;//*(p+j);
                    c=c*c;
                // }
        }
    }

    return 0;
}

static int filter_slice_chroma(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs)
{

    FadeContext *s = ctx->priv;
    AVFrame *frame = arg;
    int i, j, plane;
    const int width = AV_CEIL_RSHIFT(frame->width, s->hsub);
    const int height= AV_CEIL_RSHIFT(frame->height, s->vsub);
    int slice_start = (height *  jobnr   ) / nb_jobs;
    int slice_end   = FFMIN(((height * (jobnr+1)) / nb_jobs), frame->height);

    av_log(NULL, AV_LOG_INFO, "\n %d of %d [filter_slice_chroma] start %d end %d, width %d, height %d",jobnr, nb_jobs, slice_start, slice_end, width, height);

    for (plane = 1; plane < 3; plane++) {
        for (i = slice_start; i < slice_end; i++) {
            uint8_t *p = frame->data[plane] + i * frame->linesize[plane];
            for (j = 0; j < width; j++) {
                /* 8421367 = ((128 << 1) + 1) << 15. It is an integer
                 * representation of 128.5. The .5 is for rounding
                 * purposes. */
                if(j>frame->width/2) {
                    *(p+j) = *(p+j-frame->width/2);
                } else {
                    *(p+j) = *(p+j);
                }
            }
        }
    }

    return 0;
}

static int filter_slice_alpha(AVFilterContext *ctx, void *arg, int jobnr,
                              int nb_jobs)
{
    av_log(NULL, AV_LOG_INFO, "\nfilter_slice_alpha");

    FadeContext *s = ctx->priv;
    AVFrame *frame = arg;
    int plane = s->is_packed_rgb ? 0 : A;
    int slice_start = (frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (frame->height * (jobnr+1)) / nb_jobs;
    int i, j;

    av_log(NULL, AV_LOG_INFO, "\n %d of %d [filter_slice_alpha] start %d end %d",jobnr, nb_jobs, slice_start, slice_end);

    for (i = slice_start; i < slice_end; i++) {
        uint8_t *p = frame->data[plane] + i * frame->linesize[plane] + s->is_packed_rgb*s->rgba_map[A];
        // int step = s->is_packed_rgb ? 4 : 1;
        for (j = 0; j < frame->width; j++) {
            /* s->factor is using 16 lower-order bits for decimal
             * places. 32768 = 1 << 15, it is an integer representation
             * of 0.5 and is for rounding. */
                // if(j>frame->width/2) {
                    // *(p+j) = *(p+j-frame->width/2);
                // } else {
                    *(p+j) = *(p+j);
                // }
        }
    }

    return 0;
}

static void copyPixelsFromFishToRect(AVFrame *fish_frame, AVFrame *rect_frame, int x, int y, int layer, float width, float height) {

    float theta, phi, r, r2;
    float FOV =(float)PI/180 * 180;
    float FOV2 = (float)PI/180 * 180;
    // float width = rect_frame->width;
    // float height = rect_frame->height;

    // Polar angles
    theta = PI * (x / width - 0.5); // -pi/2 to pi/2
    phi = PI * (y / height - 0.5);  // -pi/2 to pi/2

    // Vector in 3D space
    float psph_x = cos(phi) * sin(theta);
    float psph_y = cos(phi) * cos(theta);
    float psph_z = sin(phi) ;//* cos(theta);

    // Calculate fisheye angle and radius
    theta = atan2(psph_z,psph_x);
    phi = atan2(sqrt(psph_x*psph_x+psph_z*psph_z),psph_y);

    r = width * phi / FOV;
    r2 = height * phi / FOV2;

    // Pixel in fisheye space
    int pfish_x = 0.5 * width + r * cos(theta);
    int pfish_y = 0.5 * height + r2 * sin(theta);

    // uint8_t *rect0 = rect_frame->data[0] + y * rect_frame->linesize[0];
    // uint8_t *rect1 = rect_frame->data[1] + y * rect_frame->linesize[1];
    // uint8_t *rect2 = rect_frame->data[2] + y * rect_frame->linesize[2];

    // uint8_t *fish0 = fish_frame->data[0] + pfish_y * fish_frame->linesize[0];
    // uint8_t *fish1 = fish_frame->data[1] + pfish_y * fish_frame->linesize[1];
    // uint8_t *fish2 = fish_frame->data[2] + pfish_y * fish_frame->linesize[2];

    // *(rect0+x) = *(fish0+pfish_x); 
    // *(rect1+(int)(x/2)) = *(fish1+(int)(pfish_x/2)) ; 
    // *(rect2+(int)(x/2)) = *(fish2+(int)(pfish_x/2)) ; 


    /// ---
 
    int fish_linesize = fish_frame->linesize[layer];
    int rect_linesize = rect_frame->linesize[layer];

    (rect_frame->data[layer])[x + y * rect_linesize] = (fish_frame->data[layer])[pfish_x + pfish_y * fish_linesize];

}

static void filter_slice_from_sphere_to_rect(AVFilterContext *ctx, void *arg, 
                              int jobnr,
                              int nb_jobs){

    struct ThreadPayload *payload = arg;

    AVFrame *fish_frame = payload->sphere;
    AVFrame *rect_frame = payload->square;
    int slice_start = (fish_frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (fish_frame->height * (jobnr+1)) / nb_jobs;

    int width = fish_frame->width;
    int height = fish_frame->height;

    int x, y;
    
    int move = 300;

    int layer = 0;

        int fish_linesize = fish_frame->linesize[layer];
        int rect_linesize = rect_frame->linesize[layer];

        for (y = slice_start; y < slice_end ; y++) {
            for (x = 0; x < width; x++) {

                // if(j > payload->w * payload->bpp/2){
                    // (rect_frame->data[0])[(int)y*width + x] = (fish_frame->data[0])[(y*width) + x];
                    (rect_frame->data[1])[(int)y*(width)/2 + x/2] = slice_start ;//(fish_frame->data[1])[(y/2*width/2) + x/2];
                    // (rect_frame->data[2])[(int)y/2*(width)/2 + x/2] = slice_start ;//(fish_frame->data[1])[(y/2*width/2) + x/2];

                    // size.total = size.width * size.height;
                    // y = yuv[position.y * size.width + position.x];
                    // u = yuv[(position.y / 2) * (size.width / 2) + (position.x / 2) + size.total];
                    // v = yuv[(position.y / 2) * (size.width / 2) + (position.x / 2) + size.total + (size.total / 4)];

                // } else {
                    // (rect_frame->data[layer])[(int)(i/2)*rect_linesize + (int)(j/2)] = 50;
                // }

                // copyPixelsFromFishToRect(original_frame, frame, i, j, 0, payload->w, payload->h);
            }
        }

    // layer = 1;

    //     fish_linesize = fish_frame->linesize[layer];
    //     rect_linesize = rect_frame->linesize[layer];

    //     for (i = slice_start; i < (slice_end) ; i++) {
    //         for (j = 0; j < (payload->w * payload->bpp - move); j++) {

    //             // if(j > payload->w * payload->bpp/2){
    //                 (rect_frame->data[layer])[(int)(i*2)*rect_linesize + j] = 0 ; //(fish_frame->data[layer])[(i*fish_linesize) + j+move/2];
    //                 // (rect_frame->data[layer])[(int)i*rect_linesize + 1+j*2] = (fish_frame->data[layer])[(i*fish_linesize) + 1+j*2+move/2];
    //             // } else {
    //                 // (rect_frame->data[layer])[(int)(i/2)*rect_linesize + (int)(j/2)] = 50;
    //             // }

    //             // copyPixelsFromFishToRect(original_frame, frame, i, j, 0, payload->w, payload->h);
    //         }
    //     }

    // const int width = AV_CEIL_RSHIFT(frame->width, payload->hsub);
    // const int height= AV_CEIL_RSHIFT(frame->height, payload->vsub);
    // slice_start = (height *  jobnr   ) / nb_jobs;
    // slice_end   = FFMIN(((height * (jobnr+1)) / nb_jobs), frame->height);

    // // if(original_frame->data[1]){

    //     // av_log(NULL, AV_LOG_INFO, "\n jobnr: %d, layer 1. slice_start: %d, slice_end: %d, width: %d", jobnr, slice_start, slice_end, payload->cw);
    //     for (i = slice_start; i < slice_end; ++i) {
    //         for (j = 0; j < width; ++j) {
    //             // av_log(NULL, AV_LOG_INFO, "\n x: %d y: %d", i, j);
    //             copyPixelsFromFishToRect(original_frame, frame, i, j, 1, payload->cw, payload->ch);
    //         }
    //     }

        // for (i = slice_start; i < slice_end; ++i) {
        //     for (j = 0; j < payload->cw; ++j) {
        //         // av_log(NULL, AV_LOG_INFO, "\n x: %d y: %d", i, j);
        //         copyPixelsFromFishToRect(original_frame, frame, i, j, 2, payload->cw, payload->ch);
        //     }
        // }

    // }

    // if(original_frame->data[2]){

    //     for (i = slice_start; i < slice_end; ++i) {
    //         for (j = 0; j < 1000; ++j) {
    //             // av_log(NULL, AV_LOG_INFO, "\n x: %d y: %d, jobnr: %d layer 2. slice_start: %d, slice_end: %d, width: %d", i, j, jobnr, slice_start, slice_end, payload->cw);
    //             copyPixelsFromFishToRect(original_frame, frame, i, j, 2, payload->cw, payload->ch);
    //         }
    //     }

    // }
    // av_frame_free(&original_frame);


}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{

    AVFilterContext *ctx = inlink->dst;

    int planes = 1 ;//av_pix_fmt_count_planes(frame->format);
    // av_log(NULL, AV_LOG_INFO, "\n planes: %d", av_pix_fmt_count_planes(frame->format));
    // av_log(NULL, AV_LOG_INFO, "\n linesize0: %d", (frame->linesize[0]));
    // av_log(NULL, AV_LOG_INFO, "\n linesize1: %d", (frame->linesize[1]));
    // av_log(NULL, AV_LOG_INFO, "\n linesize2: %d", (frame->linesize[2]));

    double frame_timestamp = frame->pts == AV_NOPTS_VALUE ? -1 : frame->pts * av_q2d(inlink->time_base);
    // av_log(NULL, AV_LOG_INFO, "\nfilter_frame: timestamp %f", frame_timestamp);

    AVFilterLink *outlink = inlink->dst->outputs[0];

    // AVFrame *original_frame = ff_get_video_buffer(outlink, frame->width, frame->height);
    
    // av_frame_copy(original_frame, frame);
    // av_frame_copy_props(original_frame, frame);


    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    int bpp = av_get_bits_per_pixel(desc) >> 3;
    // int planes = desc->nb_components;
    int w = inlink->w;
    int h = inlink->h;
    int hsub = desc->log2_chroma_w;
    int vsub = desc->log2_chroma_h;
    int cw = AV_CEIL_RSHIFT(frame->width, hsub);
    int ch = AV_CEIL_RSHIFT(frame->height, vsub);

    // av_log(NULL, AV_LOG_INFO, "\n linesize: %d, w: %d, h: %d, hsub: %d, vsub: %d, cw: %d, ch: %d, bpp: %d", (frame->linesize[2]), w, h, hsub, vsub, cw, ch, bpp);
    // av_log(NULL, AV_LOG_INFO, "\n layer0: %d", frame->data[0]);
    // av_log(NULL, AV_LOG_INFO, "\n layer1: %d", frame->data[1]);
    // av_log(NULL, AV_LOG_INFO, "\n layer2: %d", frame->data[2]);

    // struct ThreadPayload payload = { original_frame, frame, w, h, hsub, vsub, cw, ch, bpp, planes };

    // ctx->internal->execute(ctx, filter_slice_from_sphere_to_rect, &payload, NULL,
    //                     FFMIN(frame->height, ff_filter_get_nb_threads(ctx)));

    int x, y;
    int line_width = frame->linesize[1];
    int luma_line_width = frame->linesize[0];
    int chroma_line_width = frame->linesize[1];

    for (y = 0; y < frame->height ; y++) {
        // av_log(NULL, AV_LOG_INFO, "\n------\n");

        for (x = 0; x < luma_line_width; x++) {
            
            // av_log(NULL, AV_LOG_INFO, " %d ", (frame->data[1])[(int)y*(w) + x]);
            if(y > frame->height/2) {
                *(frame->data[0] + y * luma_line_width + x) = 0 ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            } 
            // if(x < line_width/2) {
                // *(frame->data[2] + y/2 * line_width + x) = 0 ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            // } 

        }

        for (x = 0; x < chroma_line_width; x++) {
            
            // av_log(NULL, AV_LOG_INFO, " %d ", (frame->data[1])[(int)y*(w) + x]);
            if(x > chroma_line_width/2) {
                *(frame->data[1] + y/2 * chroma_line_width + x) = 0 ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            } 
            if(x < chroma_line_width/2) {
                *(frame->data[2] + y/2 * chroma_line_width + x) = 0 ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            } 

        }
        // if(j > payload->w * payload->bpp/2){
            // (frame->data[0])[(int)y*w + x] = (original_frame->data[0])[(y*w) + x];
            // (frame->data[1])[(int)y/2*(w)/2 + x/8] = (y/h)*100 ; //y+x ;//(fish_frame->data[1])[(y/2*width/2) + x/2];
            // (rect_frame->data[2])[(int)y/2*(width)/2 + x/2] = slice_start ;//(fish_frame->data[1])[(y/2*width/2) + x/2];

            // size.total = size.width * size.height;
            // y = yuv[position.y * size.width + position.x];
            // u = yuv[(position.y / 2) * (size.width / 2) + (position.x / 2) + size.total];
            // v = yuv[(position.y / 2) * (size.width / 2) + (position.x / 2) + size.total + (size.total / 4)];

        // } else {
            // (rect_frame->data[layer])[(int)(i/2)*rect_linesize + (int)(j/2)] = 50;
        // }

        // copyPixelsFromFishToRect(original_frame, frame, i, j, 0, payload->w, payload->h);
    // }
}


    return ff_filter_frame(inlink->dst->outputs[0], frame);
}


#define OFFSET(x) offsetof(FadeContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption fade_options[] = {
    { "type", "'in' or 'out' for fade-in/fade-out", OFFSET(type), AV_OPT_TYPE_INT, { .i64 = FADE_IN }, FADE_IN, FADE_OUT, FLAGS, "type" },
    { "t",    "'in' or 'out' for fade-in/fade-out", OFFSET(type), AV_OPT_TYPE_INT, { .i64 = FADE_IN }, FADE_IN, FADE_OUT, FLAGS, "type" },
        { "in",  "fade-in",  0, AV_OPT_TYPE_CONST, { .i64 = FADE_IN },  .unit = "type" },
        { "out", "fade-out", 0, AV_OPT_TYPE_CONST, { .i64 = FADE_OUT }, .unit = "type" },
    { "start_frame", "Number of the first frame to which to apply the effect.",
                                                    OFFSET(start_frame), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, FLAGS },
    { "s",           "Number of the first frame to which to apply the effect.",
                                                    OFFSET(start_frame), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, FLAGS },
    { "nb_frames",   "Number of frames to which the effect should be applied.",
                                                    OFFSET(nb_frames),   AV_OPT_TYPE_INT, { .i64 = 25 }, 0, INT_MAX, FLAGS },
    { "n",           "Number of frames to which the effect should be applied.",
                                                    OFFSET(nb_frames),   AV_OPT_TYPE_INT, { .i64 = 25 }, 0, INT_MAX, FLAGS },
    { "alpha",       "fade alpha if it is available on the input", OFFSET(alpha),       AV_OPT_TYPE_BOOL, {.i64 = 0    }, 0,       1, FLAGS },
    { "start_time",  "Number of seconds of the beginning of the effect.",
                                                    OFFSET(start_time),  AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    { "st",          "Number of seconds of the beginning of the effect.",
                                                    OFFSET(start_time),  AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    { "duration",    "Duration of the effect in seconds.",
                                                    OFFSET(duration),    AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    { "d",           "Duration of the effect in seconds.",
                                                    OFFSET(duration),    AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    { "color",       "set color",                   OFFSET(color_rgba),  AV_OPT_TYPE_COLOR,    {.str = "black"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { "c",           "set color",                   OFFSET(color_rgba),  AV_OPT_TYPE_COLOR,    {.str = "black"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(fade);

static const AVFilterPad avfilter_vf_fade_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .config_props   = config_props,
        .filter_frame   = filter_frame,
        .needs_writable = 1,
    },
    { NULL }
};

static const AVFilterPad avfilter_vf_fade_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_fisheye = {
    .name          = "fisheye",
    .description   = NULL_IF_CONFIG_SMALL("Convert fisheye to rectangular video."),
    .init          = init,
    .priv_size     = sizeof(FadeContext),
    .priv_class    = &fade_class,
    .query_formats = query_formats,
    .inputs        = avfilter_vf_fade_inputs,
    .outputs       = avfilter_vf_fade_outputs,
    .flags         = AVFILTER_FLAG_SLICE_THREADS,
};
