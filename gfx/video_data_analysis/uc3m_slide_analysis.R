# video data animation
# 1570 - 2010
# 790 - 920
library(tidyr)
library(dplyr)
library(ggplot2)
library(imager)
library(animation)

## Plot first frame of video
frame_names <- list.files("/home/statimatician/Desktop/Supplementary material/figures_code/video_population_data_analysis/WalkByShop1front/",full.names = T)

fff <- load.image(frame_names[1])
#par()
png(filename = "frame1_video.png", width = 480, height = 440)
plot(fff, axes = F, mar = c(.1, .1, .1, 0.1), mgp = c(0, 0, 0))
dev.off()

## Plot first frame of video in greyscale
frame_names <- list.files("/home/statimatician/Desktop/Supplementary material/figures_code/video_population_data_analysis/WalkByShop1front/",full.names = T)

par(mar = c(.1, .1, .1, 0.1), mgp = c(2, 0.7, 0))
png(filename = "frame1_video_grey.png", width = 480, height = 440)
plot(grayscale(fff), axes = F)
dev.off()


## Plot first frame of video functional data
load(file = "/home/statimatician/Desktop/Supplementary material/figures_code/video_population_data_analysis/vidmatrix.RData")

par(mar = c(.1, .1, .1, 0.1), mgp = c(2, 0.7, 0))
png(filename = "frame1_video_grey_function.png", width = 896, height = 504)
matplot(vidmatrix[,1],
        type = "l",
        col = "grey51", 
        xlab = "pixel number", 
        ylab = "pixel value (greyscale)")
dev.off()


## GIF of first part of video
fast_indices <- read.csv("fast_indices.csv")

animate.walkbyshop_original <- function(frame_names = frame_names){
  #oopt <- ani.options(interval = 0.05, nmax = 2360)
  for (framei in 790:920) {
    dev.hold()
    cat(":: Working on frame on frame: ", framei, "\n"  )
    fff <- load.image(frame_names[framei])
    par(mar = c(.1, .1, .1, 0.1), mgp = c(2, 0.7, 0))
    plot(fff, axes = F)
    ani.pause()
  }
}


ani.options(interval = 0.04, ani.dev = "png", ani.type = "png")
saveGIF({animate.walkbyshop_original(frame_names)},
  movie.name = "animation_walkbyshop1.gif",
  img.name = "Rplot",
  convert = "magick", clean = TRUE,
  ani.height = 440, ani.width = 480)


## GIF of indices of first part of video
animate.walkbyshop <- function(fast_indices = fast_indices, 
                               frame_names = frame_names){
  #oopt <- ani.options(interval = 0.05, nmax = 2360)
  for (framei in 790:920) {
    dev.hold()
    cat(":: Working on frame on frame: ", framei, "\n"  )
    dtplt1 <- arrange(fast_indices, magnitude)
    p1 <- ggplot(data = dtplt1, aes(y = magnitude, x = 1:2360)) +
      geom_point( color = "grey81") +
      geom_point(data = dtplt1[dtplt1$id == framei,  ], 
                 aes(y = magnitude, x = which(dtplt1$id == framei)), 
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Magnitude") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    dtplt2 <- arrange(fast_indices, amplitude)
    p2 <- ggplot(data = dtplt2,
                 aes(y = amplitude, x = 1:2360)) + 
      geom_point( color = "grey81") +
      geom_point(data = dtplt2[dtplt2$id == framei,  ], 
                 aes(y = amplitude, x = which(dtplt2$id == framei)), 
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Amplitude")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    dtplt3 <- arrange(fast_indices, shape)
    p3 <- ggplot(data = dtplt3,
                 aes(y = shape, x = 1:2360)) + 
      geom_point(color = "grey81") +
      geom_point(data = dtplt3[dtplt3$id == framei,  ], 
                 aes(y = shape, x = which(dtplt3$id == framei)), 
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Shape") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    fff <- load.image(frame_names[framei]) %>% grayscale %>% as.data.frame
    
    p4 <- ggplot(fff, aes(x, y)) +
      geom_raster(aes(fill = value)) +
      ggtitle("Video") +
      xlab("")+
      ylab("")+
      scale_y_continuous(expand=c(0,0), trans = scales::reverse_trans()) +
      scale_x_continuous(expand=c(0,0)) +
      scale_fill_gradient(low="black", high="white")+
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    gridExtra::grid.arrange(p1, p2, p3, p4)
    ani.pause()
  }
}


# ani.options(interval = 0.04, ani.dev = "png", ani.type = "png")
# saveGIF({animate.walkbyshop(fast_indices, frame_names)},
#         movie.name = "animation_walkbyshop1_indices.gif",
#         img.name = "Rplot",
#         convert = "magick", clean = TRUE,
#         ani.height = 504, ani.width = 896)

ani.options(interval = 0.04, ani.dev = "png", ani.type = "png")
saveVideo({animate.walkbyshop(fast_indices, frame_names)},
  video.name = "animation_walkbyshop1_indices.mp4",
  img.name = "Rplot",
  ffmpeg = ani.options("ffmpeg"),
  other.opts = if (grepl("[.]mp4$", video.name)) "-pix_fmt yuv420p",
  ani.height = 648, ani.width = 1152)





## GIF of second part of video

animate.walkbyshop_original2 <- function(frame_names = frame_names){
  #oopt <- ani.options(interval = 0.05, nmax = 2360)
  for (framei in 1860:2000) {
    dev.hold()
    cat(":: Working on frame on frame: ", framei, "\n"  )
    fff <- load.image(frame_names[framei])
    par(mar = c(.1, .1, .1, 0.1), mgp = c(2, 0.7, 0))
    plot(fff, axes = F)
    ani.pause()
  }
}


ani.options(interval = 0.04, ani.dev = "png", ani.type = "png")
saveGIF({animate.walkbyshop_original2(frame_names)},
        movie.name = "animation_walkbyshop2.gif",
        img.name = "Rplot",
        convert = "magick", clean = TRUE,
        ani.height = 440, ani.width = 480)

## GIF of indices of second part of video
animate.walkbyshop2 <- function(fast_indices = fast_indices, 
                                frame_names = frame_names){
  #oopt <- ani.options(interval = 0.05, nmax = 2360)
  for (framei in 1860:2000) {
    dev.hold()
    cat(":: Working on frame on frame: ", framei, "\n"  )
    dtplt1 <- arrange(fast_indices, magnitude)
    p1 <- ggplot(data = dtplt1, aes(y = magnitude, x = 1:2360)) +
      geom_point( color = "grey81") +
      geom_point(data = dtplt1[dtplt1$id == framei,  ], 
                 aes(y = magnitude, x = which(dtplt1$id == framei)),
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Magnitude") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    dtplt2 <- arrange(fast_indices, amplitude)
    p2 <- ggplot(data = dtplt2,
                 aes(y = amplitude, x = 1:2360)) + 
      geom_point( color = "grey81") +
      geom_point(data = dtplt2[dtplt2$id == framei,  ], 
                 aes(y = amplitude, x = which(dtplt2$id == framei)),
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Amplitude")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    dtplt3 <- arrange(fast_indices, shape)
    p3 <- ggplot(data = dtplt3,
                 aes(y = shape, x = 1:2360)) + 
      geom_point(color = "grey81") +
      geom_point(data = dtplt3[dtplt3$id == framei,  ], 
                 aes(y = shape, x = which(dtplt3$id == framei)),
                 size = 4)+
      ylab("sorted index") +
      xlab("") +
      ggtitle("Shape") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    
    fff <- load.image(frame_names[framei]) %>% grayscale %>% as.data.frame
    
    p4 <- ggplot(fff, aes(x, y)) +
      geom_raster(aes(fill = value)) +
      ggtitle("Video") +
      xlab("")+
      ylab("")+
      scale_y_continuous(expand=c(0,0), trans = scales::reverse_trans()) +
      scale_x_continuous(expand=c(0,0)) +
      scale_fill_gradient(low="black", high="white")+
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, 
                                      size = 32))
    gridExtra::grid.arrange(p1, p2, p3, p4)
    ani.pause()
  }
}


ani.options(interval = 0.04, ani.dev = "png", ani.type = "png")
saveVideo({animate.walkbyshop2(fast_indices, frame_names)},
          video.name = "animation_walkbyshop2_indices.mp4",
          img.name = "Rplot",
          ffmpeg = ani.options("ffmpeg"),
          other.opts = if (grepl("[.]mp4$", video.name)) "-pix_fmt yuv420p",
          ani.height = 648, ani.width = 1152)



## Plot frame 2110 of video in greyscale
frame_names <- list.files("/home/statimatician/Desktop/Supplementary material/figures_code/video_population_data_analysis/WalkByShop1front/",full.names = T)
fff <- load.image(frame_names[2110])
par(mar = c(.1, .1, .1, 0.1), mgp = c(2, 0.7, 0))
png(filename = "frame2110_video_grey.png", width = 480, height = 440)
plot(grayscale(fff), axes = F)
dev.off()
