
currentApi<<-1
newSearchTwitter<-function(arg1,arg2,arg3,arg4)
{
a<-getCurRateLimitInfo()
if(as.integer(a$remaining[as.integer(which(a$resource=="/search/tweets"))])<13)
{
  setnewapi()
}
return (searchTwitter(searchString = arg1,n=arg2,resultType = arg3,geocode =arg4))
}

setnewapi<-function()
{
  #get new ap
  a<-read.csv("data/tokens.csv",stringsAsFactors = F)
  while(T)
  {

    if(currentApi>0&&currentApi<length(a$name))
    {
      currentApi<<-currentApi+1
    }
    else
    {
      currentApi<<-1
      
    }
  auth(currentApi)
  b<-getCurRateLimitInfo()
  if(as.integer(b$remaining[as.integer(which(b$resource=="/search/tweets"))])>13)
  {
    break
  }
  }}