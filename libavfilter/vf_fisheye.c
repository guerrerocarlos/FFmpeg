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

typedef struct FisheyeContext {
    const AVClass *class;
    int angle;
    int margin_left;
    int margin_right;
    int margin_top;
    int margin_bottom;
    int x_center_offset;
    int y_center_offset;

} FisheyeContext;

typedef struct ThreadPayload {
    AVFrame *original_frame;
    AVFrame *frame;
    int luma_line_width;
    int chroma_line_width;
    int planes;
};

static av_cold int init(AVFilterContext *ctx)
{

    av_log(NULL, AV_LOG_INFO, "\n Initializing fisheye filter...");

    FisheyeContext *s = ctx->priv;
    // av_log(NULL, AV_LOG_INFO, "\n Angle: %d", s->angle);

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV444P,      AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV420P,      AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV410P,      AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_YUVJ420P,
        // AV_PIX_FMT_GRAY8,
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
    FisheyeContext *s = inlink->dst->priv;
    // const AVPixFmtDescriptor *pixdesc = av_pix_fmt_desc_get(inlink->format);

    return 0;
}

// Fisheye to spherical conversion
static void mapFromFisheyeToSquare(int x, int y, float width, float height, int *pfish_x, int *pfish_y, FisheyeContext *s) {
    // inspired on fish2sphere function in http://paulbourke.net/dome/fish2/
    
    // Assumes the fisheye image is square, centered, and the circle fills the image.
    // Output (spherical) image should have 2:1 aspect.
    // Strange (but helpful) that atan() == atan2(), normally they are different.

    float theta, phi, r, r2;

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

    r = width * phi / PI;
    r2 = height * phi / PI;

    // Pixel in fisheye space
    *pfish_x = 0.5 * width + r * cos(theta);
    *pfish_y = 0.5 * height + r2 * sin(theta);

}

static void filter_slice_from_sphere_to_rect(AVFilterContext *ctx, void *arg, 
                              int jobnr,
                              int nb_jobs){

    FisheyeContext *s = ctx->priv;

    struct ThreadPayload *payload = arg;

    AVFrame *original_frame = payload->original_frame;
    AVFrame *frame = payload->frame;

    int luma_line_width = payload->luma_line_width;
    int chroma_line_width = payload->chroma_line_width;

    int slice_start = (original_frame->height *  jobnr   ) / nb_jobs;
    int slice_end   = (original_frame->height * (jobnr+1)) / nb_jobs;

    int x, y;
    int square_x;
    int square_y;
    int chroma_x;

    for (y = slice_start; y < slice_end ; y++) {
        for (x = 0; x < luma_line_width; x++) {
            mapFromFisheyeToSquare(x, y, luma_line_width, frame->height, &square_x, &square_y, s);
            *(frame->data[0] + y * luma_line_width + x) = *(original_frame->data[0] + square_y * luma_line_width + square_x) ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            if(frame->data[1]) {
                chroma_x = x * chroma_line_width / luma_line_width;
                square_x = square_x  * chroma_line_width / luma_line_width;
                *(frame->data[1] + y/2 * chroma_line_width + chroma_x) = *(original_frame->data[1] + square_y/2 * chroma_line_width + square_x) ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
                *(frame->data[2] + y/2 * chroma_line_width + chroma_x) = *(original_frame->data[2] + square_y/2 * chroma_line_width + square_x) ; //x>w/2 ? 0 :(original_frame->data[0])[y * frame->width + x];
            }
        }
    }


}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{

    AVFilterContext *ctx = inlink->dst;

    int planes = av_pix_fmt_count_planes(frame->format);

    double frame_timestamp = frame->pts == AV_NOPTS_VALUE ? -1 : frame->pts * av_q2d(inlink->time_base);

    AVFilterLink *outlink = inlink->dst->outputs[0];

    AVFrame *original_frame = ff_get_video_buffer(outlink, frame->width, frame->height);
    av_frame_copy(original_frame, frame);
    av_frame_copy_props(original_frame, frame);

    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    int luma_line_width = frame->linesize[0];
    int chroma_line_width = frame->linesize[1];

    struct ThreadPayload payload = { original_frame, frame, luma_line_width, chroma_line_width, planes };

    ctx->internal->execute(ctx, filter_slice_from_sphere_to_rect, &payload, NULL,
                        FFMIN(frame->height, ff_filter_get_nb_threads(ctx)));

    av_frame_free(&original_frame);
    return ff_filter_frame(inlink->dst->outputs[0], frame);
}


#define OFFSET(x) offsetof(FisheyeContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption fade_options[] = {
    // Options will be added based on user's requests //

    // { "angle",           "Fisheye angular (default: 180).",
    //                                                 OFFSET(angle), AV_OPT_TYPE_INT, { .i64 = 180 }, 0, 360, FLAGS },
    // { "margin_left",   "Number of frames to which the effect should be applied.",
    //                                                 OFFSET(margin_left),   AV_OPT_TYPE_INT, { .i64 = 25 }, 0, INT_MAX, FLAGS },
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
    .priv_size     = sizeof(FisheyeContext),
    .priv_class    = &fade_class,
    .query_formats = query_formats,
    .inputs        = avfilter_vf_fade_inputs,
    .outputs       = avfilter_vf_fade_outputs,
    .flags         = AVFILTER_FLAG_SLICE_THREADS,
};
