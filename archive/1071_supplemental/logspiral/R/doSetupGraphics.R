doSetupGraphics <-
function (high = 4, wide = 4, pt.size = 8) 
{
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -25, ypos = 0)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -450, ypos = 0)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -25, ypos = -30)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -450, ypos = -30)
    invisible()
}
