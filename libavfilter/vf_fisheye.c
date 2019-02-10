/*
 * Copyright (c) 2018 Carlos Guerrero
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
 * fisheye to equirectangular projection video converter
 * based heavily on vf_fade.c by Brandon Mintern
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

#define PI 3.1415926536
#define SAMPLES 2704

int SAMPLESH, SAMPLESW;

typedef struct FisheyeContext
{
    const AVClass *class;
    int angle;
    int margin_left;
    int margin_right;
    int margin_top;
    int margin_bottom;
    int x_center_offset;
    int y_center_offset;
    int merge;
    int width;
    int SAMPLESW;
    int SAMPLESH;
} FisheyeContext;

typedef struct ThreadPayload
{
    AVFrame *original_frame;
    AVFrame *frame;
    int luma_line_width;
    int chroma_line_width;
    int SAMPLESH;
    int SAMPLESW;
};

int *projections; //[2][SAMPLES][SAMPLES];
int *projections2;
// int yProjections [2][ SAMPLES ][ SAMPLES ];

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV410P, AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_YUVJ420P,
        AV_PIX_FMT_NONE};

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

const static enum AVPixelFormat studio_level_pix_fmts[] = {
    AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUV422P, AV_PIX_FMT_YUV420P,
    AV_PIX_FMT_YUV411P, AV_PIX_FMT_YUV410P,
    AV_PIX_FMT_YUV440P,
    AV_PIX_FMT_NONE};

// Fisheye to spherical conversion
static void mapFromFisheyeToSquare(int x, int y, float width, float height, int *pfish_x, int *pfish_y)
{
    // inspired on fish2sphere function in http://paulbourke.net/dome/fish2/

    // Assumes the fisheye image is square, centered, and the circle fills the image.
    // Output (spherical) image should have 2:1 aspect.
    // Strange (but helpful) that atan() == atan2(), normally they are different.

    float alpha, beta, rx, ry;
    float yrad, xrad;

    // Polar angles
    yrad = PI * (y / height - 0.5);  // -pi/2 to pi/2
    xrad = PI * (x / width - 0.5); // -pi/2 to pi/2

    // Vector in 3D space
    float psph_x = cos(yrad) * sin(xrad);
    float psph_y = cos(yrad) * cos(xrad);
    float psph_z = sin(yrad); //* cos(theta);

    // Calculate fisheye angle and radius
    alpha = atan2(psph_z, psph_x);
    beta = atan2(sqrt(psph_x * psph_x + psph_z * psph_z), psph_y);

    rx =  ( width  / 2 ) * ( beta - 0.02 * beta * beta ) / ( PI / 2 );
    // rx =  ( width  / 2 ) * beta / ( PI / 2 );
    ry =  ( height / 2 ) * beta / ( PI / 2 );

    // adjustments
    rx = rx * 100 / 100;

    // Pixel in fisheye space
    *pfish_x = 0.5 * width  +   rx * cos(alpha) - 20; 
    *pfish_y = 0.5 * height +   ry * sin(alpha);
}

static void mergePixels(int x, float starting_x, float finishing_x, int merge_width, int *delta_x)
{   
    // int starting_x = width/4; //- merge_width/2 ;
    // int left_center = width / 4;
    int width = finishing_x - starting_x;
    *delta_x = - merge_width * ( x - starting_x ) / (width + merge_width); // * (5.4e-6 * pow(x_, 2) + 3.9e-7 * pow(x_, 3)) ;
    
    // if(x % 2) {
    // *origin_x = x + (x - starting_x);
    // }else{
    //     *origin_x = starting_x + merge_width - (x - starting_x);
    // }
    // *origin_y = y + 1;

    // if(x % 2) {
    //     *origin_x = width / 2 - merge_width/2 + x-width/2;
    //     *origin_y = y;
    // } else {
    //     *origin_x = width / 2 - merge_width/2 + x-width/2;
    // }
    // *origin_y = y+1;
}

static av_cold int init(AVFilterContext *ctx)
{

    // av_log(NULL, AV_LOG_INFO, "\n Fisheye mapping finished.");
    // av_log(NULL, AV_LOG_INFO, "\n for x:%d, y:%d, px:%d, py:%d.");
    av_log(NULL, AV_LOG_INFO, "\n Fisheye mapping finished.");

    return 0;
}

static int config_props(AVFilterLink *inlink)
{
    // FisheyeContext *s = inlink->dst->priv;
    AVFilterContext *ctx = inlink->src;
    FisheyeContext *s = ctx->priv;
    // AVRational time_base = ctx->inputs[0]->time_base;
    // AVRational frame_rate = ctx->inputs[0]->frame_rate;
    // AVRational sar = ctx->inputs[0]->sample_aspect_ratio;

    int SAMPLESH = ctx->inputs[0]->h;
    int SAMPLESW = ctx->inputs[0]->w;

    s->SAMPLESH = SAMPLESH;
    s->SAMPLESW = SAMPLESW;

    av_log(NULL, AV_LOG_INFO, "\n Initializing fisheye filter... s->merge?: %d, s->width: %d, SAMPLESW: %d, SAMPLESH: %d", s->merge, s->width, SAMPLESW, SAMPLESH);

    // FisheyeContext *s = ctx->priv;

    if (s->merge)
    {
        av_log(NULL, AV_LOG_INFO, "\n Merging!");
        projections = malloc(2 * SAMPLESW * SAMPLESH * sizeof(int));
        projections2 = malloc(2 * SAMPLESW * SAMPLESH * sizeof(int));

        int x, y;
        int delta_x;
        float vertical_delta; 
        float vertical_multiplier;

        for (y = 0; y < SAMPLESH; y++)
        {
            for (x = 0; x < SAMPLESW; x++)
            {
                vertical_delta = abs( y - SAMPLESH/2 ) * 100000 / (SAMPLESH/2);
                if (x > (SAMPLESW / 4) && x < (SAMPLESW / 2) + s->width)
                {
                    // av_log(NULL, AV_LOG_INFO, "\n cleaning!");

                    mergePixels(x, SAMPLESW/4, SAMPLESW/2, s->width, &delta_x);
                    *(projections                       + SAMPLESW * y + x) = x + delta_x * (1 + 2* vertical_delta / 100000);
                    *(projections + SAMPLESH * SAMPLESW + SAMPLESW * y + x) = y;

                }
                else
                {
                    *(projections + SAMPLESW * y + x) = x;
                    *(projections + SAMPLESH * SAMPLESW + SAMPLESW * y + x) = y;
                }

                if (x > (SAMPLESW / 2 - s->width) && x < (SAMPLESW * 3 / 4) ) {
                    mergePixels(x, SAMPLESW * 3 / 4, SAMPLESW / 2 - s->width, s->width, &delta_x);
                    *(projections2                       + SAMPLESW * y + x) = x - delta_x * (1 + 2* vertical_delta / 100000);
                    *(projections2 + SAMPLESH * SAMPLESW + SAMPLESW * y + x) = y;
                } else {
                    *(projections2 + SAMPLESW * y + x) = x;
                    *(projections2 + SAMPLESH * SAMPLESW + SAMPLESW * y + x) = y;
                }
                // av_log(NULL, AV_LOG_INFO, "\n calculating...");
                // av_log(NULL, AV_LOG_INFO, "\n for x:%d, y:%d, px:%d, py:%d", x, y, *(SAMPLESW, SAMPLESH, projections + SAMPLESH * y + x), *(projections + SAMPLESH * SAMPLESW + SAMPLESH * y + x) );
            }
        }
    }
    else
    {
        av_log(NULL, AV_LOG_INFO, "\n Squaring!");
        projections = malloc(2 * SAMPLESW * SAMPLESH * sizeof(int));

        int x, y;
        // int width = s->width;
        for (y = 0; y < SAMPLESH; y++)
        {
            for (x = 0; x < SAMPLESW; x++)
            {
                // av_log(NULL, AV_LOG_INFO, "\n calculating...");
                mapFromFisheyeToSquare(x, y, SAMPLESW, SAMPLESH, projections + SAMPLESW * y + x, projections + SAMPLESH * SAMPLESW + SAMPLESW * y + x);
                // av_log(NULL, AV_LOG_INFO, "\n for x:%d, y:%d, px:%d, py:%d", x, y, *(SAMPLESW, SAMPLESH, projections + SAMPLESH * y + x), *(projections + SAMPLESH * SAMPLESW + SAMPLESH * y + x) );
            }
        }
    }

    av_log(NULL, AV_LOG_INFO, "\n Calculations Finished! ");

    return 0;
}

static void filter_slice_from_sphere_to_rect(AVFilterContext *ctx, void *arg,
                                             int jobnr,
                                             int nb_jobs)
{

    FisheyeContext *s = ctx->priv;

    struct ThreadPayload *payload = arg;

    AVFrame *original_frame = payload->original_frame;
    AVFrame *frame = payload->frame;

    int luma_line_width = payload->luma_line_width;
    int chroma_line_width = payload->chroma_line_width;

    int SAMPLESH = payload->SAMPLESH;
    int SAMPLESW = payload->SAMPLESW;
    int slice_start = (SAMPLESH * jobnr) / nb_jobs;
    int slice_end = (SAMPLESH * (jobnr + 1)) / nb_jobs;

    int x, y;
    int square_x, square_y, square_x2, square_y2;
    int chroma_x;
    int saved_x, input_x, saved_y, input_y;
    int merger_x;

    // av_log(NULL, AV_LOG_INFO, "\n -- slice_start %d, slice_end %d, SAMPLESH %d, SAMPLESW %d", slice_start, slice_end, SAMPLESH, SAMPLESW );
    // if(s->merge) {
    //     SAMPLESW = s->width;
    // }
    for (y = slice_start; y < slice_end; y++)
    {
        for (x = 0; x < SAMPLESW; x++)
        {
            // input_x = x; //* SAMPLESW / luma_line_width;
            // input_y = y; //* SAMPLESH / original_frame->height;
            if (s->merge) {

                square_x = *(projections + SAMPLESW * y + x);
                square_y = *(projections + SAMPLESW * SAMPLESH + SAMPLESW * y + x);

                square_x2 = *(projections2 + SAMPLESW * y + x);
                square_y2 = *(projections2 + SAMPLESW * SAMPLESH + SAMPLESW * y + x);

                if( x <= SAMPLESW / 2 ) {
                    *(frame->data[0] + y * luma_line_width + x) = *(original_frame->data[0] + square_y * luma_line_width + square_x);
                } 

                // if( x > (SAMPLESW / 2 - s->width) && x < (SAMPLESW / 2 + s->width) ) {
                //     merger_x = ( x - (SAMPLESW / 2 - s->width) ) * 100 / (2 * s->width);
                //     // av_log(NULL, AV_LOG_INFO, "\n x %d, SAMPLESW / 2 - s->width: %d, merger_x %d, SAMPLESW %d", x, SAMPLESW / 2 - s->width, merger_x, SAMPLESW);

                //     *(frame->data[0] + y * luma_line_width + x) = ( *(original_frame->data[0] + square_y * luma_line_width + square_x) * (100 - merger_x) + *(original_frame->data[0] + square_y2 * luma_line_width + square_x2) * merger_x ) / 100;
                //     *(frame->data[0] + y * luma_line_width + x) = ( *(original_frame->data[0] + square_y * luma_line_width + square_x) * (100 - merger_x) + *(original_frame->data[0] + square_y2 * luma_line_width + square_x2) * merger_x ) / 100;
                //     // *(frame->data[0] + y * luma_line_width + x) = *(original_frame->data[0] + square_y * luma_line_width + square_x);
                // }

                if( x >= SAMPLESW / 2 ) {
                    *(frame->data[0] + y * luma_line_width + x) = *(original_frame->data[0] + square_y2 * luma_line_width + square_x2);
                } 


            } else {
                square_x = *(projections + SAMPLESW * y + x);
                square_y = *(projections + SAMPLESW * SAMPLESH + SAMPLESW * y + x);
                *(frame->data[0] + y * luma_line_width + x) = *(original_frame->data[0] + square_y * luma_line_width + square_x);
            }

            // square_x = saved_x * luma_line_width / SAMPLESW;
            // square_y = saved_y * original_frame->height / SAMPLESH;

            // av_log(NULL, AV_LOG_INFO, "\n -- x:%d, y:%d, px:%d, py:%d", x, y, square_x, square_y );


            if (frame->data[2])
            {
                chroma_x = x * chroma_line_width / luma_line_width;
                square_x = square_x * chroma_line_width / luma_line_width;
                *(frame->data[1] + y / 2 * chroma_line_width + chroma_x) = *(original_frame->data[1] + square_y / 2 * chroma_line_width + square_x);
                *(frame->data[2] + y / 2 * chroma_line_width + chroma_x) = *(original_frame->data[2] + square_y / 2 * chroma_line_width + square_x);
            }
        }
    }
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{

    AVFilterContext *ctx = inlink->dst;
    FisheyeContext *s = ctx->priv;

    // int planes = av_pix_fmt_count_planes(frame->format);

    // double frame_timestamp = frame->pts == AV_NOPTS_VALUE ? -1 : frame->pts * av_q2d(inlink->time_base);

    AVFilterLink *outlink = inlink->dst->outputs[0];

    AVFrame *out = ff_get_video_buffer(outlink, frame->width, frame->height);
    if (!out)
    {
        av_frame_free(&frame);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, frame);

    // const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    int luma_line_width = frame->linesize[0];
    int chroma_line_width = frame->linesize[1];

    // av_log(NULL, AV_LOG_INFO, "\n -- luma_line_width %d, chroma_line_width %d, SAMPLESH %d, SAMPLESW %d", luma_line_width, chroma_line_width, SAMPLESH, SAMPLESW );

    struct ThreadPayload payload = {frame, out, luma_line_width, chroma_line_width, s->SAMPLESH, s->SAMPLESW};

    ctx->internal->execute(ctx, filter_slice_from_sphere_to_rect, &payload, NULL,
                           FFMIN(frame->height, ff_filter_get_nb_threads(ctx)));

    av_frame_free(&frame);
    return ff_filter_frame(inlink->dst->outputs[0], out);
}

#define OFFSET(x) offsetof(FisheyeContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_FILTERING_PARAM

static const AVOption fade_options[] = {
    // Options will be added based on user's requests //

    {"merge", "Merges two equirectangulars.", OFFSET(merge), AV_OPT_TYPE_BOOL, {.i64 = 0}, 0, 1, .flags = FLAGS},
    {"width", "Width of the action.", OFFSET(width), AV_OPT_TYPE_INT, {.i64 = 100}, 0, INT_MAX, FLAGS},
    // { "margin_right",           "Number of frames to which the effect should be applied.",
    //                                                 OFFSET(margin_right),   AV_OPT_TYPE_INT, { .i64 = 25 }, 0, INT_MAX, FLAGS },
    // { "margin_top",  "Number of seconds of the beginning of the effect.",
    //                                                 OFFSET(margin_top),  AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    // { "margin_bottom",  "Number of seconds of the beginning of the effect.",
    //                                                 OFFSET(margin_bottom),  AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    // { "x_center_offset",          "Number of seconds of the beginning of the effect.",
    //                                                 OFFSET(x_center_offset),  AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    // { "y_center_offset",    "Duration of the effect in seconds.",
    //                                                 OFFSET(y_center_offset),    AV_OPT_TYPE_DURATION, {.i64 = 0. }, 0, INT32_MAX, FLAGS },
    {NULL}};

AVFILTER_DEFINE_CLASS(fade);

static const AVFilterPad avfilter_vf_fade_inputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .needs_writable = 1,
    },
    {NULL}};

static const AVFilterPad avfilter_vf_fade_outputs[] = {
    {
        .name = "default",
        .config_props = config_props,
        .type = AVMEDIA_TYPE_VIDEO,
    },
    {NULL}};

AVFilter ff_vf_fisheye = {
    .name = "fisheye",
    .description = NULL_IF_CONFIG_SMALL("Convert fisheye to rectangular video."),
    .init = init,
    .priv_size = sizeof(FisheyeContext),
    .priv_class = &fade_class,
    .query_formats = query_formats,
    .inputs = avfilter_vf_fade_inputs,
    .outputs = avfilter_vf_fade_outputs,
    .flags = AVFILTER_FLAG_SLICE_THREADS,
};
