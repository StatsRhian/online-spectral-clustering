library("mvnfast")


nSphereCoordsInv <- function(dim,coords){
  
  #This function takes a cartesian coordinate vector of length n and
  #outputs the radius and n-1 angle parameters needed to parametrise a
  #point in n-dimensional space.
  
  #Make ``coords" a column vector
  coords = as.vector(coords)
  
  # check dimensions
  if(dim!=length(coords)){
    stop("dimension mismatch")
  }
  
  #Create element-wise square of coords for use in computing the angles.
  coordsSq = coords^2
  
  #Calculate the radius using standard formula
  radius = sqrt(sum(coordsSq))
  
  #Preallocate the angles parameter vector
  a = rep(0,dim-1)
  
  #Calculate the first dim-2 angles, which can vary between 0 and pi
  for(i in 1:(dim-2)){
    a[i] = acos(coords[i]/( sqrt(sum(coordsSq[i:dim]))))
    if( is.nan(a[i])==T){
      a[i] = acos(0)
    }
  }
  
  #Calculate the final angles parameter, which can vary between 0 and 2*pi
  if( coords[dim]>=0){
    a[dim-1] = acos( coords[dim-1] / sqrt(coordsSq[dim] + coordsSq[dim-1]));
  }else{
    a[dim-1] = 2*pi - acos( coords[dim-1] / sqrt(coordsSq[dim] + coordsSq[dim-1]));
  }
  
  #Again, correct for NaN errors with the final angles parameter
  if( is.nan(a[dim-1])==T){
    if( coords[dim]>=0){
      a[dim-1] = acos(0);
    }else{
      a[dim-1] = 2*pi - acos(0);
    }
  }
  
  #Output angles
  angles = a
  out = list(angles=angles,radius,radius)
  return(out)
  
}

#################################################################################################################################

nSphereCoords <- function(dim,angles,radius){
  
  #This function takes a vector of n-1 angles and a radius, and outputs
  #the equivalent cartesian coordinates in n-dimensional space.
  
  #Note the parametrisation should be such that
  # angle[1] should the parameter which varies between 0 and 2*pi
  
  #Check input is correct
  if(length(angles)!=(dim-1)){
    stop("angles must be of length dim-1")
  }
  
  #Create vector which will store the cartesian coordinates
  x = rep(0,dim)
  
  #Loop over all dimensions and calculate the coordinates using
  #n-dimensional spherical coordinates.
  for(i in 1:dim){
    if(i==dim){
      x[i] = radius * prod(sin(angles))
    }else if(i==1){
      x[i] = radius * cos(angles[1])
    }else{
      x[i] = radius * prod(sin(angles[1:(i-1)])) * cos(angles[i])
    }
  }
  
  out = x
  return(x)
}


##################################################################################################################################

orbitingGaussians <- function(dim,rate,radius,numPoints,fudge=0.075){
  
  #Function to create two points orbiting eachother on an n-sphere. Two
  #matrices of dimensions (numPoints x dim) are produced giving the position
  #of the two centres at each time point.
  
  #dim = dimension of data
  #1/rate = the approx number of time points to complete 1 rotation
  #radius = radius of hysphere
  #numPoints = number of time points
  
  if(dim < 2){
    stop("Require dim=>2")
  }
  twoPi = 2*pi
  
  #Initialise centres opposite each other through the origin
  class1 = nSphereCoordsInv(dim,rep(1,dim))
  c1Angles = class1$angles
  rad      = class1$radius
  class2 = nSphereCoordsInv(dim,rep(-1,dim))
  c2Angles = class2$angles
  
  #Preallocate arrays for storing the position of the centres at each
  #timestep
  c1 = matrix(0,numPoints,dim)
  c2 = matrix(0,numPoints,dim)
  
  #Loop over each timepoint
  for(i in 1:numPoints){
    #Get cartesian coords
    c1[i,] = nSphereCoords(dim ,c1Angles,radius)
    c2[i,] = nSphereCoords(dim ,c2Angles,radius)
    
    #Update final angle parameter (i.e. the only one which can vary between
    # %0 and 2*pi). This parameter should be updated at a slower rate than all
    #other parameters in order avoid the centres moving too rapidly over the
    #surface of the hypersphere. Hence we use a 0.075 "fudge factor".
    
    c1Angles[dim-1] = (c1Angles[dim-1]+2*pi*rate*fudge) %% twoPi
    c2Angles[dim-1] = (c2Angles[dim-1]+2*pi*rate*fudge) %% twoPi
    
    #Update the remaining angle parameters.
    c1Angles[1:(dim-2)] = (c1Angles[1:(dim-2)]+2*pi*rate) %% twoPi
    c2Angles[1:(dim-2)] = (c2Angles[1:(dim-2)]-2*pi*rate) %% twoPi
    
  }
  
  #Produce output
  centres1=c1
  centres2=c2
  out = list(centres1=centres1,centres2=centres2)
  return(out)
}
#########################################################################################################################

generateData <- function(dim,numPoints,rate,radius,ProbClassOne,covar,fudge=0.075){
  
  #Function to simulate slowly-moving concept drift data by
  #drawing classes from two gaussian distributions whose centre points
  #are orbiting each other on the surface of an n-sphere.
  
  #dim = dimensions of data points
  #numPnts = Number of data/time points
  #rate = rate of rotation NOTE: Recommend rate<<0.2
  #radius = radius of hypersphere
  #ProbClassOn = The overall probability of a class 1 instance being observed
  #var = specify variance of gaussians.
  
  if(ProbClassOne > 1 || ProbClassOne <0){
    stop("class probabilties must be in [0,1]")
  }
  
  if(numPoints < 20){
    stop("more points needed")
  }
  
  if(ProbClassOne<0.05 || ProbClassOne>0.95){
    stop("try a more balanced probability split")
  }
  
  #First generate the position of the centres of the two gaussian
  #distributions at each time point.
  centres = orbitingGaussians(dim,rate,radius,numPoints,fudge)
  
  # Prepare data and labels vector
  data = matrix(NA,numPoints,dim)
  labels = rep(NA,numPoints)
  
  # class priors
  prior = c(ProbClassOne,1-ProbClassOne)
  
  for(i in 1:numPoints){
    # Generate label
    l = sample(c(1,2),1,prob=prior)
    labels[i] = l
    
    if(l==1){
      data[i,] = rmvn(1, centres$centres1[i,], covar)
    }else{
      data[i,] = rmvn(1, centres$centres2[i,], covar)
    }
  }
  
  out = list(centres=centres,data=data,labels=labels)
  return(out)
}

############################## Parameter settings #############################################
#dim = 3            # Dimensionality of the data
#rate = 1/2000      # 1/rate = approximately the number of points needed for a class mean to complete 1 rotation
#fudge = 0.05      # Used to ensure points don't skip over the surface too fast. Default is normally suitable

#numPoints= 2000    # Number of data points
#radius = 1         # Radius of hypersphere
#ProbClassOne = 0.5 # Probaility of class twp = 1 - ProbClassOne
#covar = diag(dim)*0.005 # Noise covariance matrix

# Run
#out = generateData(dim=dim,numPoints=numPoints,rate=rate,radius=radius,ProbClassOne=ProbClassOne,covar=covar)

# Plot
#library("car")
#library("rgl")
#scatter3d(out$data[,1],out$data[,2],out$data[,3],surface=F,ellipsoid = F,
#          xlab="x",ylab="y",zlab="z",axis.scales = F,sphere.size=1,groups=as.factor(out$labels))

###############################################################################################################

















