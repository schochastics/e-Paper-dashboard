hsl2rgb  <-  function(h, s, l) {
  i  <-  floor(h * 6);
  f  <-  h * 6 - i
  p  <-  l * (1 - s)
  q  <-  l * (1 - f * s)
  t  <-  l * (1 - (1 - f) * s)
  if(i%%6==0){
    c(l,t,p)
  } else if(i%%6==1){ 
    c(q,l,p)
  } else if(i%%6==2){
    c(p,l,t)
  } else if(i%%6==3){
    c(p,q,l)
  } else if(i%%6==4){
    c(t,p,l)
  } else if(i%%6==5){
    c(l,p,q)
  } 
}

get_body <- function(mask,width,height){
  for(x in 1:width){
    for(y in 1:height){
      if(mask[y,x]==1){
        mask[y,x] <- round(runif(1)>0.5)
      } else if(mask[y,x]==2){
        if(runif(1)>0.5){
          mask[y,x]==1
        } else{
          mask[y,x]==-1
        }
      }
    }
  }
  mask
}

mirror_body <- function(mask,width){
  cbind(mask,mask[,width:1])
  # rbind(mask,mask[])
}

generate_edges <- function(mask,width,height){
  for(x in 1:width){
    for(y in 1:height){
      if(mask[y,x]==1){
        if((x-1)>=1){
          if(mask[y,x-1]==0){
            mask[y,x-1] <- -1
          }
        } 
        if((x+1)<=width){
          if(mask[y,x+1]==0){
            mask[y,x+1] <- -1
          }
        } 
        if((y-1)>=1){ 
          if(mask[y-1,x]==0){
            mask[y-1,x] <- -1
          }
        }
        if((y+1)<=height){
          if(mask[y+1,x]==0){
            mask[y+1,x] <- -1
          }
        }
      }
    }
  }
  mask
}

apply_colors <- function(mask,
                         is_vertical_gradient,
                         color_variations,
                         colored,
                         brightness_noise,
                         saturation,
                         edge_brightness,
                         hue,
                         height,
                         width){
  
  mask_rgb <- matrix("",height,width)
  if(is_vertical_gradient){
    ulen <- height
    vlen <- width
  } else{
    ulen <- width
    vlen <- height
  }
  
  for(u in 1:ulen){
    is_new_color <- abs(sum(runif(3)*2-1))/3
    
    if(is_new_color>(1-color_variations)){
      hue <- runif(1)
    }
    
    for(v in 1:vlen){
      if(is_vertical_gradient){
        val <- mask[u,v]
      } else{
        val <- mask[v,u]
      }
      
      rgb <- c("r"=1,"g"=1,"b"=1)
      if(val!=0){
        if(colored){
          brightness <- sin((u/ulen)*pi) * (1-brightness_noise) + runif(1) * brightness_noise
          rgb_vals <- hsl2rgb(hue,saturation,brightness)
          rgb[1] <- rgb_vals[1]
          rgb[2] <- rgb_vals[2]
          rgb[3] <- rgb_vals[3]
          
          if(val==-1){
            rgb <- rgb * edge_brightness
          }
        } else{
          if(val==-1){
            rgb <- c("r"=0,"g"=0,"b"=0)
          }
        }
      }
      if(is_vertical_gradient){
        mask_rgb[u,v] <- rgb(rgb[["r"]],rgb[["g"]],rgb[["b"]])
      } else{
        mask_rgb[v,u] <- rgb(rgb[["r"]],rgb[["g"]],rgb[["b"]])
      }
    }
  }
  mask_rgb
}

generate_sprite <- function(mask,
                            color_variations = 0.2,
                            colored = TRUE,
                            brightness_noise = 0.3,
                            edge_brightness = 0.3,
                            saturation = 0.2,
                            mirror=FALSE){
  width <- ncol(mask)
  height <- nrow(mask)
  
  is_vertical_gradient <- runif(1) > 0.5
  saturation <- max(min(c(runif(1) * saturation,1)),0)
  hue <- runif(1)
  
  A <- get_body(mask,width,height)
  if(mirror){
    A <- mirror_body(A,width)
    width <- ncol(A)
    height <- nrow(A)
  }
  A <- generate_edges(A,width,height)
  A <- apply_colors(A,is_vertical_gradient,color_variations,colored,brightness_noise,
                    saturation,edge_brightness,hue,height,width)
  A  
}



dragon_small <-  matrix(c(
  0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,1,1,1,1,0,0,0,0,
  0,0,0,1,1,2,2,1,1,0,0,0,
  0,0,1,1,1,2,2,1,1,1,0,0,
  0,0,0,0,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,0,
  0,0,0,0,1,1,1,1,1,1,1,0,
  0,0,1,1,1,1,1,1,1,1,0,0,
  0,0,0,1,1,1,1,1,1,0,0,0,
  0,0,0,0,1,1,1,1,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0),12,12,byrow=TRUE)

dragon_large <-  matrix(c(
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,
  0,0,0,1,1,2,2,2,1,1,1,1,0,0,0,0,
  0,0,1,1,2,2,2,2,2,1,1,1,1,0,0,0,
  0,1,1,1,1,2,2,2,1,1,1,1,1,1,0,0,
  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,
  0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
  0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
  0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,
  0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),16,16,byrow=TRUE)

robot <- matrix(c(
  0, 0, 0, 0,
  0, 1, 1, 1,
  0, 1, 2, 2,
  0, 0, 1, 2,
  0, 0, 0, 2,
  1, 1, 1, 2,
  0, 1, 1, 2,
  0, 0, 0, 2,
  0, 0, 0, 2,
  0, 1, 2, 2,
  1, 1, 0, 0),11,4,byrow=TRUE)


space <- matrix(c(
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 1,-1,
  0, 0, 0, 1, 1,-1,
  0, 0, 0, 1, 1,-1,
  0, 0, 1, 1, 1,-1,
  0, 1, 1, 1, 2, 2,
  0, 1, 1, 1, 2, 2,
  0, 1, 1, 1, 2, 2,
  0, 1, 1, 1, 1,-1,
  0, 0, 0, 1, 1, 1,
  0, 0, 0, 0, 0, 0),12,6,byrow=TRUE) 

dragon_top <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 1, 1, 1, 1,
  0, 0, 1, 1, 1, 1, 1, 1,
  0, 0, 1, 1, 1, 1, 1, 1,
  0, 1, 1, 0, 0, 0, 1, 1,
  0, 1, 0, 0, 0, 0, 1, 1,
  0, 1, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 0, 0),16,8,byrow=TRUE) 