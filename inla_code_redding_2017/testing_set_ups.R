library(raster)
library(dismo)
library(rJava)
library(XML)
library(gstat)
library(INLA)
library(gbm)
library(nlme)
library(R.utils)
library(maptools)
library(spatstat)

source("E:\\Dropbox\\R_scripts\\maxentvariableselection2.r")
source("E:\\Dropbox\\R_scripts\\sim_data4.R")
source("E:\\Dropbox\\R_scripts\\stepINLAd.R")
source("E:\\Dropbox\\R_scripts\\maxent_aic.R")
e <- simpleError("test error")

model_res2<-NULL

model_res2<-NULL
for (s in 1:1000){
	if(s==1){z=1}
	print("starting SIMULATION");print(Sys.time())

	clustn<-sample(seq(from=10,to=150,by=10),1)
	sampn<-sample(seq(from=50,to=500,by=75),1)
	bias1<-sample(seq(from=1,to=10,by=1),1)
	clust_s<-sample(seq(from=1,to=5,by=1),1)
	resol1<-1
	int2=FALSE ##include interactions in simulate

	cc<-sim_data(resol=resol1,range_complexity=15,samples_in_range=sampn,countries=4,reporting_bias_level=bias1,clust=clust_s,no_of_points_per_cluster=clustn,npredict=5,training_samples=100,thresh=0.7,jit=10,interactions=int2,nonrand=FALSE)
	print("FINISHED SIMULATION");print(Sys.time())
	##set objects
	datax<-cc$samples  ### biased and clumped set of samples 
	datax<-SpatialPointsDataFrame(datax,data=data.frame(Presence=rep(1,nrow(datax))))
	predictors<-cc$predictors  ### random predictor layers
	sps<-cc$actual_range ### actual range you want to recreate
	sps[is.na(sps)]<-0
	aform1<-cc$formula1

	###assess coverage of points
	sps2<-sps
	sps2[sps2==0]<-NA
	projection(sps2)<-projection(raster())
	xy<-randomPoints(sps2,1000)
	ext1<-chull(x=xy[,1],y=xy[,2])
	xy2<-Polygons(list(Polygon(rbind(xy[ext1,],xy[ext1[1],]))),ID=1)
	xy3<-SpatialPolygons(list(xy2))
	projection(xy3)<-projection(raster())

	##chull samples
	ext1<-chull(x=coordinates(datax)[,1],y=coordinates(datax)[,2])
	xy2x<-Polygons(list(Polygon(rbind(coordinates(datax)[ext1,],coordinates(datax)[ext1[1],]))),ID=1)
	xy3x<-SpatialPolygons(list(xy2x))
	projection(xy3x)<-projection(raster())
	coverage1<-(xy3x@polygons[[1]]@area)/(xy3@polygons[[1]]@area)

	##clustering of points
	clustered1<-clarkevans(as(datax,"ppp"))[2][[1]]

	###real test data
	datat1<-as.data.frame(randomPoints(predictors,nrow(datax)*2))
	coordinates(datat1)<-~x+y
	datat1<-SpatialPointsDataFrame(datat1,data=data.frame(Presence=raster::extract(sps,datat1)))
	datat<-datat1[datat1$Presence==1,]
	datatb<-datat1[datat1$Presence==0,]

	##psuedo absence points
	dataa<-as.data.frame(randomPoints(raster(predictors),nrow(datax)*2))
	coordinates(dataa)<-~x+y
	dataa<-SpatialPointsDataFrame(dataax,data=data.frame(Presence=rep(0,length(dataax))))

	##weighted psuedo absence
	temp1<-raster(predictors)
	freq1<-rasterize(datax,temp1,field="Presence",sum)
	freq2<-aggregate(freq1,fact=resol1*3)
	freq3<-raster::resample(freq2,predictors,method='ngb')
	freq3<-freq3/max(values(freq3),na.rm=T)
	freq3[is.na(values(freq3))]<-0.1
	dataa2<-as.data.frame(randomPoints(freq3,nrow(datax)*2,prob=T))
	coordinates(dataa2)<-~x+y
	dataa3<-SpatialPointsDataFrame(dataa2,data=data.frame(Presence=rep(0,length(dataa2))))

	###testing for different set ups
	for (qq in 1:10){

	if(qq==1){spat1=FALSE; spinla=TRUE;int1=FALSE;thin1=FALSE;step1=FALSE} ##spatially uncorrelated
	if(qq==2){spat1=FALSE; spinla=TRUE;int1=FALSE;thin1=TRUE;step1=FALSE} ##spatial thinning + spatially uncorrelated
	if(qq==3){spat1=TRUE; spinla=TRUE;int1=FALSE;thin1=TRUE;step1=FALSE} ##spatial thinning + spatially uncorrelated
	if(qq==4){spat1=TRUE; spinla=TRUE;int1=FALSE;thin1=FALSE;step1=FALSE} ##spatial thinning + spatially uncorrelated
	if(qq==5){spat1=FALSE; spinla=FALSE;int1=FALSE;thin1=FALSE;step1=FALSE} ##spatially uncorrelated +no spatial term
	if(qq==6){spat1=FALSE; spinla=FALSE;int1=FALSE;thin1=TRUE;step1=FALSE} ##spatial thinning + spatially uncorrelated +no spatial term
	if(qq==7){spat1=TRUE; spinla=FALSE;int1=FALSE;thin1=TRUE;step1=FALSE} ##spatial thinning + spatially uncorrelated +no spatial term
	if(qq==8){spat1=TRUE; spinla=FALSE;int1=FALSE;thin1=FALSE;step1=FALSE} ##spatial thinning + spatially uncorrelated +no spatial term
	if(qq==9){spat1=FALSE; spinla=TRUE;int1=FALSE;thin1=FALSE;step1=TRUE} ## replicate 1 but with stepwise covariates where possible
	if(qq==10){spat1=FALSE; spinla=TRUE;int1=TRUE;thin1=FALSE;step1=FALSE} ## replicate 1 but with interactions in formulae where possible

	##change depending on set up
	if(spat1==TRUE){dataa<-dataa3}else{dataa<-dataaX}##spatially correlated absence
	if(thin1==TRUE){
		asas<-cellFromXY(freq2,datax)
		datax<-datax[sample(1:nrow(datax),nrow(datax),replace=F),]
		datax2<-datax[!duplicated(asas),]
		if(nrow(datax2@data)>25){datax<-datax2}else{thin1=FALSE}
		}

	group <- dismo::kfold(datax, 5)
	pres_train <- datax[group != 1, ]
	pres_test <- datax[group == 1, ]
	group <- dismo::kfold(dataa, 5)
	backg_train <- dataa[group != 1, ]
	backg_test <- dataa[group == 1, ]

	## five variations per set up
	for (ff in 1:5){
	################# MAXENT ################

		if(qq<5){
			print("MAXENT");print(Sys.time())

			if(step1==TRUE){
				#step maxent prepartion
				num<-paste(sample(c(0:9, letters, LETTERS),size=12, replace=TRUE),collapse="")
				dir.create(paste("E:/test_bed/",num,"/",sep=""))
				setwd(paste("E:/test_bed/",num,"/",sep=""))
				#dir.create("./ascii/")
				for(sq in 1:nlayers(predictors)){l1<-subset(predictors,sq);writeRaster(l1,format="ascii",file=paste("./",names(l1),".asc",sep=""),overwrite=T)}
				datax2<-coordinates(datax)
				names(datax2)<-c("Longitude","Latitude")
				write.csv(datax2,file="./casex.csv",row.names = F)
				dataa2<-coordinates(dataa)
				names(dataa2)<-c("Longitude","Latitude")
				write.csv(dataa2,file="./targetted_absencesx.csv",row.names = F)
				#step maxent
				res1<-maxentvariableselection(filen_predictors="./",filen_presence_lonlat="./casex.csv",filen_absence_lonlat="./targetted_absencesx.csv",additionalargs=("noproduct nothreshold noautofeature"),maxent =("E:/Documents/R/win-library/3.2/dismo/java/maxent.jar"),outdir=("./"),contributionthreshold <- 1,correlationthreshold <- 0.7,betamultiplier=1:5)
				beta1<-res1$betaAIC$betamultiplier[1]
				i1<-res1$variablesAIC[!is.na(res1$variablesAIC$Correlation),"Test"]
				include<-names(predictors)[names(predictors) %in% i1]
			}else{
				#non-step maxent
				i1<-names(predictors)
				include<-names(predictors)[names(predictors) %in% i1]
				beta1=seq(1,21,5)[ff]
			}

			### set up inputs
			for (xx in 1:50){
				setwd("E:\\Dropbox\\MEE_write_up")
				facts<-NULL
				argsX<-c("nolinear","noquadratic","noproduct","nothreshold","nohinge")
				compl1<-sample(1:(length(argsX)-1),1)
				args1X<-sample(argsX,compl1,replace=F)
				args1X<-args1X[order(args1X)]
				args1X2<-c("noautofeature",args1X)
				betas<-paste("betamultiplier=",beta1,sep="")
				if(xx==1){
					args1<-betas
				}else{
					args1<-c(paste("betamultiplier=",beta1,sep=""),args1X2)
				}

				### RUN MAXENT
				maxtime<-system.time(xm <- dismo::maxent(subset(predictors,include), removeDuplicates=T, p=coordinates(pres_train),a=coordinates(backg_train),factors=NULL,args=args1))

				###predict external test and loop for ensemble
				predictxm<-predict(xm,predictors)
				predictxm3<-calc(predictxm,mean,na.rm=T)
				p2<-raster::extract(predictxm3,datat)
				a2<-raster::extract(predictxm3,datatb)
				e3b <-dismo::evaluate(p=p2,a=a2)
				AUCens<-e3b@auc

				maxAIC<-AICmaxent(model = xm, p = rbind(pres_train,backg_train), sc = predictxm)[2]

				#### evaluate model as before but using training set
				e2 <- dismo::evaluate(pres_test, backg_test, xm, predictors)

				### evaluate model as before but using external set
				e3 <- dismo::evaluate(datat, datatb, xm, predictors)
				layersx<-as.data.frame(t(data.frame(layers=names(predictors),used=names(predictors) %in% i1)))[2,]
				names(layersx)<-names(predictors)
				rownames(layersx)<-z

				model_res<-data.frame(version=s,replicate=NA,model=z,setup=qq,clustn,sampn,bias1,clust_s,coverage1,clustered1,layersx[1,],AUC_int=e2@auc,AUC_ext=e3@auc,AUC_pred=e3@auc,AUC_ens=AUCens,AIC=maxAIC,arguements=paste(args1,collapse=" "),maxent_compl=compl1,beta=beta1,method="MAXENT",comp_time=max(maxtime,na.rm=T),cut_off=NA,tree_compl=NA,learn_rate=NA,bag_frac=NA,n.trees=NA,cor1=cor(values(sps),values(predictxm),use= "pairwise.complete.obs"),spatial=spat1,sp_term=spinla,form_inter=int1,simul_inter=int2,choose_corr=step1)
				if(is.null(model_res2)){model_res2<-model_res}else{model_res2<-rbind(model_res2,model_res)}
				z=z+1

			}#end of xx loop
	
			## pretend loop if doesn't converge
			for(ss in 1){
				############# BOOSTED REGRESSION TREES #############
				print("BRT");print(Sys.time())
				presvals<-data.frame(raster::extract(predictors, pres_train))
				presvals2<-data.frame(raster::extract(predictors, pres_test))
				absvals <- data.frame(raster::extract(predictors, backg_train))
				absvals2 <- data.frame(raster::extract(predictors, backg_test))
				tabsvals <- data.frame(raster::extract(predictors, datat))
				tabsvals2 <- data.frame(raster::extract(predictors, datatb))

				###prepare data
				pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
				sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
				pb <- c(rep(1, nrow(presvals2)), rep(0, nrow(absvals2)))
				testdata <- data.frame(cbind(pb, rbind(presvals2, absvals2)))
				pb <- c(rep(1, nrow(tabsvals)), rep(0, nrow(tabsvals2)))
				ttestdata <- data.frame(cbind(pb, rbind(tabsvals, tabsvals2)))

				test1<-NULL;test2<-NULL
				##set arguments
				compl1<-sample(3:8,1)
				bag_frac<-sample(c(0.75,0.5,0.65),1)
				LR<-sample(c(0.001,0.005,0.01,0.05),1)

				ff=1;zzz=1
				while(fff==1 & zzz<=10){
					test2<-tryCatch(dismo::gbm.step(data=sdmdata , gbm.x = 2:ncol(sdmdata), gbm.y = 1,max.trees = 10000,family = "bernoulli", tree.complexity = compl1,learning.rate = LR, bag.fraction = bag_frac,silent=T),error=function(e) e)
					fff=0
					if(class(test2)[1]=="simpleError" | is.null(test2)|"glm.fit: algorithm did not converge" %in% names(tail(warnings(),2))){
						compl1<-sample(3:8,1)
						bag_frac<-sample(c(0.75,0.5,0.625),1)
						LR<-sample(c(0.0005,0.001,0.005,0.01,0.05,0.1),1)
						fff=1}
					zzz=zzz+1;print(zzz)
					}

				if(zzz>10){next}

				if(step1==TRUE){
					test1<-gbm.step(data=sdmdata , gbm.x = 2:ncol(sdmdata), gbm.y = 1,max.trees = 10000,family = "bernoulli", tree.complexity = 3,learning.rate = 0.001, bag.fraction = 0.75,silent=T)
					if(is.null(test1)){next}
					test1b<-gbm.simplify(test1)
					test2 <- gbm.step(data=sdmdata, gbm.x = (test1b$pred.list[[1]]), gbm.y = 1,max.trees = 10000,family = "bernoulli", tree.complexity = compl1,learning.rate = LR, bag.fraction = bag_frac,silent=T)
				}

				brt_time<-test2$gbm.call$elapsed.time.minutes
				ntrees<-sample(c(200,400,test2$gbm.call$best.trees),1)

				##evalute internal
				preds <- gbm::predict.gbm(test2, testdata,n.trees=ntrees, type="response")
				d <- cbind(testdata$pb, preds)
				pres <- d[d[,1]==1, 2]
				abs <- d[d[,1]==0, 2]
				e7 <- dismo::evaluate(p=pres, a=abs)

				##evaluate external
				preds <- gbm::predict.gbm(test2, ttestdata,n.trees=ntrees, type="response")
				d <- cbind(ttestdata$pb, preds)
				pres <- d[d[,1]==1, 2]
				abs <- d[d[,1]==0, 2]
				e8 <- dismo::evaluate(p=pres, a=abs)

				##spatial predict
				predictb<-predict(predictors, test2,n.trees=ntrees, type="response")
				predictb3<-calc(predictb,mean,na.rm=T)
				p2<-raster::extract(predictb3,datat)
				a2<-raster::extract(predictb3,datatb)
				e7b <-dismo::evaluate(p=p2,a=a2)
				AUCens<-e7b@auc

				layersx<-as.data.frame(t(data.frame(layers=names(predictors),used=names(predictors) %in% test2$gbm.call$predictor.names)))[2,]
				names(layersx)<-names(predictors)
				rownames(layersx)<-z

				model_res<-data.frame(version=s,replicate=NA,model=z,setup=qq,clustn,sampn,bias1,clust_s,coverage1,clustered1,layersx[1,],AUC_int=e7@auc,AUC_ext=e8@auc,AUC_pred=e8@auc,AUC_ens=AUCens,AIC=test2$cv.statistics$deviance.mean,arguements=NA,method="BRT",maxent_compl=NA,beta=NA,comp_time=brt_time,cut_off=NA,tree_compl=compl1,learn_rate=LR,bag_frac=bag_frac,n.trees=ntrees,cor1=cor(values(sps),values(predictb),use= "pairwise.complete.obs"),spatial=spat1,sp_term=spinla,form_inter=int1,simul_inter=int2,choose_corr=step1)
				model_res2<-rbind(model_res2,model_res)
				z=z+1
			}#end of ss mini loop
		} #end of if qq<5 if

		########### INLA ################
		print("INLA");print(Sys.time())
		for (i in 1){
			###make training data
			coords2<-rbind(pres_train,backg_train,pres_test,backg_test,datat, datatb)
			names(coords2)<-"p"
			coords2$p[((nrow(pres_train)+nrow(backg_train))+1):nrow(coords2)]<-NA

			###add training data and testing data (NAs) together
			sss<-raster::extract(predictors,coords2)
			sss<-as.data.frame(sss)
			sss$p<-coords2$p
			sss[is.na(sss)]<-0
			coords2@data<-sss

			#### sample reasonable values for mesh
			minME<-2
			maxME<-100
			co<-c(0.5,1,3,5,7)[ff]
			print(paste("Cut off value ",co))
			minOS<-1
			maxOS<-20

			mesh5<-NULL
			while(!class(mesh5)[1]=="inla.mesh"){
				meshtime<-system.time(
					mesh5<-tryCatch(evalWithTimeout({inla.mesh.2d(loc= coordinates(coords2), max.edge = c(minME, maxME), cutoff = co, offset = c(minOS, maxOS))}, timeout=20, onTimeout="warning"),error=function(e) e)
							)
				if(class(mesh5)[1]=="simpleError"){
					# capture the result of a `tasklist` system call
					after.win.tasklist <- system2( 'tasklist' , stdout = TRUE )
					after.win.tasklist <- after.win.tasklist[5:length(after.win.tasklist)]
					after.win.tasklist <-gsub("\\ +"," ",after.win.tasklist)
					after.win.tasklist <-read.table(text=after.win.tasklist,sep=" ")
					names(after.win.tasklist)<-c("Image_Name","PID","Session_Name","Session","Mem_Usage")

					# identify correct pid
					correct.pid <-after.win.tasklist$PID[after.win.tasklist$Image_Name=="FMESHE~2.EXE" | after.win.tasklist$Image_Name=="fmesher64.exe"]
					if(length(correct.pid)>0){
						for (zzz in 1:length(correct.pid)){
							# kill the same process that was loaded
							system(paste( "taskkill" , "/PID" , correct.pid[zzz],"/F" ))}
					}
					# jitter coodinates slightly
					coords2<-data.frame(x=jitter(coordinates(coords2)[,1],0.1),y=jitter(coordinates(coords2)[,2],0.1),coords2@data)
					coordinates(coords2)<-~x+y
				}#end of simple error if

			}#end of while loop

			###A Matrix
			A = inla.spde.make.A(mesh5, loc = coordinates(coords2))

			####The SPDE model is defined by
			spde <- inla.spde2.matern(mesh5, alpha=2)

			##trialsls
			coords2$trials<-1

			##stack
			stk.dat <- inla.stack(data=list(y=coords2$p),
							A=list(A,1), 
							tag='dat',
							effects=list(list(i=1:spde$n.spde),
							data.frame(Intercept=1,
							coords2@data
						)))

			## all terms from model
			termsx<-paste(paste(names(coords2@data)[names(coords2@data) %in% include],collapse="+"),"+ I(",paste(paste(names(coords2@data)[names(coords2@data) %in% include],collapse="^2)+I("),"^2)",sep=""),"+ I(",paste(paste(names(coords2@data)[names(coords2@data) %in% include],collapse="^3)+I("),"^3)",sep=""))
			termsx<-gsub(" ","",termsx)

			##second order interactions
			termsx2<-strsplit(termsx,"+",fixed=T)[[1]]
			lvls<- data.frame(p1=unlist(lapply(combn(termsx2,2,simplify=FALSE),function(x) x[1])),p2=unlist(lapply(combn(termsx2,2,simplify=FALSE),function(x) x[2])))
			lvls2<-do.call(paste, c(lvls[names(lvls)], sep=":"))

			## include interactions in formula or not
			if(int1==TRUE){
				form1<-formula(paste("y ~ 0 + Intercept +  f(i, model=spde)",termsx,paste(lvls2,collapse="+"),sep="+"))
			}else{
				form1<-formula(paste("y ~ 0 + Intercept +  f(i, model=spde)",termsx,sep="+"))
			}	

			##non spatial inla
			if(spinla==FALSE){
				form1<-formula(paste("y ~ 0 + Intercept",termsx,sep="+"))
			}
	
			res5<-tryCatch(evalWithTimeout({inla(form1,family="gaussian",control.compute=list(cpo=TRUE,dic=T,waic=T),verbose=FALSE,data=inla.stack.data(stk.dat),control.predictor=list(A=inla.stack.A(stk.dat), compute=TRUE),control.fixed = list(expand.factor.strategy = "inla"),silent=T)}, timeout=1000, onTimeout="warning"),error=function(e) e)
			if(res5[1]=="reached elapsed time limit [cpu=1000s, elapsed=1000s]"){break}
			if(class(res5)[1]=="simpleError"){break}

			if(step1==TRUE){
				res5a<-stepINLA(fam1="gaussian",dataf=coords2,invariant="0 + Intercept +  f(i, model=spde)", direction="forwards",include=1:nlayers(predictors),y="y",in_stack=stk.dat,powerl=3,inter=1,thresh=0.1,Ntrials = coords2$trials,do.inla=FALSE)
				form1<-res5a$best_formula
				res5<-tryCatch(evalWithTimeout({inla(form1,family="gaussian",control.compute=list(cpo=TRUE,dic=T,waic=T),verbose=FALSE,data=inla.stack.data(stk.dat),control.predictor=list(A=inla.stack.A(stk.dat), compute=TRUE),control.fixed = list(expand.factor.strategy = "inla"),silent=T)}, timeout=1000, onTimeout="warning"),error=function(e) e)
				if(class(res5)[1]=="simpleError"){break}
			}

			if(res5[1]=="reached elapsed time limit [cpu=1000s, elapsed=1000s]"){break}
			inlatime<-res5$cpu.used[4]+meshtime[3]

			##put it all together ## replace with function!!!!
			res1<-data.frame(row.names=rownames(res5$summary.fixed),res5$summary.fixed)
			res1$sig<-"non-sig"##is a term significant? i.e. does it include 0 in distribution
			res1$sig[(res1$X0.025quant<0 & res1$X0.975quant<0)|(res1$X0.025quant>0 & res1$X0.975quant>0)]<-"sig"
			res1$nam<-paste(rownames(res1),"*", res1$mean,sep="")
			res1$nam<-gsub("layer","predictors$layer",res1$nam,fixed=T)
			res1$nam<-gsub(":","*",res1$nam,fixed=T)
			res1$nam<-gsub("I(","(",res1$nam,fixed=T)
			res1$nam<-gsub("Intercept*","",res1$nam,fixed=T)
			rr<-paste("(",paste(res1$nam,collapse=")+("),")",sep="")
			predictx<-eval(parse(text=rr)) 
			predictx[predictx<0]<-0
			predictx<-predictx/max(values(predictx),na.rm=T)
			p2<-raster::extract(predictx,datat)
			a2<-raster::extract(predictx,datatb)
			e4b<-dismo::evaluate(p=p2,a=a2)
			AUCens<-e4b@auc

			###fixed effects testing with external testing points
			p1<-raster::extract(predictx,datat)
			a1<-raster::extract(predictx,datatb)
			e4 <-dismo::evaluate(p=p1,a=a1)
		
			##EXTRACT PREDICTED VALUES	
			c3<-as.data.frame(res5$summary.fitted.values[1:nrow(coords2), c("mean", "sd") ])
			c3$p_pred<-c3$mean#round(c3$mean,0)
			c3$p_real<-rbind(pres_train,backg_train,pres_test,backg_test,datat, datatb)$Presence
	
			###external testing points
			c4<-c3[((nrow(pres_train)+nrow(backg_train)+nrow(pres_test)+nrow(backg_test))+1):nrow(coords2),]

			##evaluate INLA output ## full predict
			e5 <-dismo::evaluate(p=c4[c4$p_real==1,"p_pred"],a=c4[c4$p_real==0,"p_pred"])

			###internal testing points
			c5<-c3[(nrow(pres_train)+nrow(backg_train))+1:(nrow(pres_train)+nrow(backg_train)+nrow(pres_test)+nrow(backg_test)),]

			##evaluate INLA output ## full predict
			e5b <-dismo::evaluate(p=c5[c5$p_real==1,"p_pred"],a=c5[c5$p_real==0,"p_pred"])

			layersx<-as.data.frame(t(data.frame(layers=names(predictors),used=names(predictors) %in% rownames(res1))))[2,]
			names(layersx)<-names(predictors)
			rownames(layersx)<-z

			model_res<-data.frame(version=s,replicate=NA,model=z,setup=qq,clustn,sampn,bias1,clust_s,coverage1,clustered1,layersx[1,],AUC_int=e5b@auc,AUC_ext=e5@auc,AUC_pred=e4@auc,AUC_ens=AUCens,AIC=res5$waic$waic,arguements=NA,method="INLA",maxent_compl=NA,beta=NA,comp_time=max(inlatime,na.rm=T),cut_off=co,tree_compl=NA,learn_rate=NA,bag_frac=NA,n.trees=NA,cor1=cor(values(sps),values(predictx),use= "pairwise.complete.obs"),spatial=spat1,sp_term=spinla,form_inter=int1,simul_inter=int2,choose_corr=step1)
			model_res2<-rbind(model_res2,model_res)
			z=z+1
			}## end of INLA pretend loop

		##end of ff loop
		}

	}#end of qq loop ## set ups

print(paste("######## s - ",s,sep=""))
}##end of main s loop (new range)

write.csv(model_res2,file=paste("E:\\Dropbox\\MEE_write_up\\maxent_inla_brt_runs10_",sample(1:10000,1),".csv",sep=""))



