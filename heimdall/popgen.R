
genpopup<-function(prof,handler,time,tweet,lat,long,radi,col){
  temp<-handler
  if(unlist(strsplit(handler,"RF"))[1]!=handler)
  {
    temp<-substr(unlist(strsplit(handler,"RF"))[1], 1, nchar(unlist(strsplit(handler,"RF"))[1])-1)
  }
return(paste(sep = "",
"  <div class='pop' style='margin:auto;border:3px solid ",col,";width:100%;border-radius:3%;padding-bottom:5px;'>
    
    <div class='image-box' style='background-color: ",col,";height:30px;border-radius:3%;'>
    
    <img src='",temp,".png' style='border:4px solid white; border-radius:50%; height:50px; width:50px;left:5px;position:relative;left:5px;top:5px;'>
    
    
    <div class='image-text' style='position:relative;float:right;text-align:right;padding-right:5px;padding-top:2px;'>
    <b style='font-weight: bold;font: 22px calibri;color:white;'>",prof,"</b>
    <br>
    <div class='image-text2' style='position:relative;float:bottom;'>
    <b style='font: 16px calibri;color:grey;'>@",handler,"</b>
    <br>
    <b style='font: 13px calibri;color:",col,";'>",time,"</b>
    <br>
    
    </div>
    </div>
    
    
    
    
    </div>
    <div class='image-tweet' style='padding-left:5px;padding-right:5px;padding-top:40px;text-align:center;'>
    <b style='font: 12px calibri;color:grey;'>",tweet,"</b>
    <br>
    <b style='font: 13px Arial Rounded MT Bold;color:LightSlateGrey;'>[",lat,",",long,",",radi," km]</b>
    <br>
    
    </div>
    
    </div>
  "
  
))
  }