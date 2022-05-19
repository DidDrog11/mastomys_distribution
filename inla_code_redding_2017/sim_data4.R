require(raster)
require(sp)
require(dismo)
require(gstat)

###testing
#for(i in 1:100){
#clustn<-sample(seq(from=10,to=150,by=10),1)
#sampn<-sample(seq(from=75,to=300,by=75),1)
#bias1<-sample(seq(from=1,to=10,by=1),1)
#clust_s<-sample(seq(from=1,to=8,by=1),1)
#resol1<-4
#int1=FALSE
#resol=resol1;range_complexity=15;samples_in_range=sampn;countries=6;reporting_bias_level=bias1;clust=clust_s;no_of_points_per_cluster=clustn;npredict=5;training_samples=100;thresh=0.7;jit=10;interactions=FALSE


sim_data<-function(resol=4,range_complexity=15,samples_in_range=50,countries=6,reporting_bias_level=5,clust=4,no_of_points_per_cluster=10,npredict=5,training_samples=100,thresh=0.6,jit=5,interactions=FALSE,nonrand=FALSE){

templ<-raster()
res(templ)<-resol

###make actual distribution as a polygon
a1<-templ
values(a1)<-0
ext1<-extent(a1)-40#c(sample(-180:-50,1),sample(180:50,1),sample(-90:-30,1),sample(90:30,1)))
pps<-randomPoints(a1,range_complexity,ext=ext1)
pps2<-as.data.frame(rbind(pps[chull(pps),],pps[chull(pps)[1],]))
coordinates(pps2)<-~x+y
p = Polygon(pps2)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

###rasterize polygon
a2<-rasterize(sps,templ,getCover=T)
a2[a2<50]<-NA
a2[a2>=50]<-1

# make reporting bias raster layer
if((countries/2)!=as.integer(countries/2)){countries=countries+1}
repeff<-templ
values(repeff)<-1
rowsx<-round(nrow(templ)/2,0)
colsx1<-round(ncol(templ)/round(countries/2,0),0)
colsx2<-seq(from=1, to=ncol(templ),by=colsx1)
colsx2<-c(colsx2,ncol(templ))
for(i in 1:round(countries/2,0)){
repeff[1:rowsx,colsx2[i]:colsx2[i+1]]<-abs(rnorm(mean=1,sd=reporting_bias_level,1))
repeff[(rowsx+1):ncol(templ),colsx2[i]:colsx2[i+1]]<-abs(rnorm(mean=1,sd=reporting_bias_level,1))
}
repeff<-repeff/max(values(repeff))

##set boundaries of grid for predictors
yy1<-templ
xy <- expand.grid(1:ncol(templ), 1:nrow(templ))
names(xy) <- c("x","y")

#### Make predictors
for (i in 1:npredict){
g.dummy <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, beta=c(1,sample(seq(from=0,to=0.01,0.001),1),sample(seq(from=0,to=0.01,0.01),1)), model=vgm(psill=sample(seq(from=0,to=0.1,0.01),1), range=sample(seq(from=1,to=50,1),1), model='Exp'), nmax=20)
yy <- predict(g.dummy, newdata=xy, nsim=1)
values(yy1)<-yy$sim1
names(yy1)<-paste("layer",i,sep="")
if(i==1){predictors<-yy1}else{predictors=stack(predictors,yy1)}
}

###make mask of real polygon
sps2<-a2
sps2[is.na(values(sps2))]<-999
sps2[sps2<999]<-NA
sps2[sps2==999]<-0

##make linear model of range
pp1<-stack(predictors,predictors^2)
zz<-jitter(raster::extract(pp1,randomPoints(a2,training_samples)),jit)
xx<-jitter(raster::extract(pp1,randomPoints(sps2,training_samples)),jit)
zx<-data.frame(pa=c(rep(1,nrow(zz)),rep(0,nrow(xx))),rbind(zz,xx))
form1<-formula(paste(names(zx)[1],"~",paste(names(zx)[2:ncol(zx)],collapse="+"),sep=""))
lm1<-lm(form1,data=zx)#,family="binomial")
if(interactions==TRUE){
lm2<-step(lm1,scope=~.^2,k=log(nrow(zx)))
}else{
lm2<-step(lm1,k=log(nrow(zx)))
}

##predict new range
newrange<-predict(pp1,lm2)
newrange[newrange<thresh]<-NA
newrange2<-newrange-thresh
newrange2<-newrange2/max(values(newrange2),na.rm=T)
newrange2[is.na(newrange2)]<-0
newrange[newrange>=thresh]<-1

###make x random clumped points
datax<-data.frame(a=-999,b=1);ccc=1
while(nrow(datax)<samples_in_range){
	#print(datax)
	##get random cell from grid
	grid<-sample(1:ncell(templ),1)
	
	## keep this starting point using probability of bias
	xy2<-xyFromCell(templ, grid,spatial=T)
	bias1<-raster::extract(repeff,xy2)
	if(sample(c(0,1),1,prob=c(1-bias1,bias1))==0){next}

	if(nonrand==TRUE){
	## keep this starting point using probability of presence
	bias1<-raster::extract(newrange2,xy2)
	if(sample(c(0,1),1,prob=c(1-bias1,bias1))==0){next}
				}
	##make window around point
	size<-round(abs(rnorm(1,mean=clust,sd=clust)))
	if(size<1){size=1}
	#if(size<(clust-2)){size=(clust-2)}
	xy<-rowColFromCell(templ, grid)
	xymin<-xy-size
	xymax<-xy+size
	xymin[xymin<0]<-0
	xymax[xymax[,1]>nrow(templ)]<-nrow(templ)
	xymax[xymax[,2]>ncol(templ)]<-ncol(templ)

	###choose random number of points
	zz<-round(abs(rnorm(1,mean=no_of_points_per_cluster,sd=no_of_points_per_cluster/3)))
      if(zz==0){next}
	if((nrow(datax)+zz)>samples_in_range){zz=samples_in_range-nrow(datax)}

	### make mask and random points within window
	templ2<-templ
	templ2[xymin[1,1]:xymax[1,1],xymin[1,2]:xymax[1,2]]<-1
	tt<-data.frame(xyFromCell(templ2,1:ncell(templ2)),values(templ2))
	tt2<-tt[!is.na(tt$values.templ2.),c(1,2)]
	tt3<-SpatialPointsDataFrame(tt2,data=tt2)
	if(min(tt3$x)==max(tt3$x)|min(tt3$y)==max(tt3$y)){ext2<-extent(tt3)+2}else{ext2<-extent(tt3)}	
	if(ext2[1]<extent(templ)[1]){ext2[1]<-extent(templ)[1]}
	if(ext2[2]>extent(templ)[2]){ext2[2]<-extent(templ)[2]}
	if(ext2[3]<extent(templ)[3]){ext2[3]<-extent(templ)[3]}
	if(ext2[4]>extent(templ)[4]){ext2[4]<-extent(templ)[4]}## make sure not bigger than world
	templ3<-raster(tt3,nrow=250,ncol=250,ext=ext2)
	values(templ3)<-1;projection(templ3)<-projection(raster())
	rp<-randomPoints(templ3,zz,tryf=100)

	##create table of results and stop at 100 points
	if(datax[1,1]==(-999)){datax<-as.data.frame(rp)}else{datax<-rbind(datax,rp)}
	qq<-raster::extract(newrange,datax)
	datax<-datax[!is.na(qq),]
	if(nrow(datax)==0){datax<-data.frame(a=-999,b=1)}
	ccc<-ccc+1;if(ccc>1000){break}
} ##end of while loop

return(list(actual_range=newrange,samples=datax,predictors=predictors,formula1=coefficients(lm2)))

}#### end function

