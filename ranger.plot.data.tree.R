######
#
# @author: Thomas Kisler
#
# Attempt at a script that plots a ranger tree in a nice way.
#
# Limitations: can only handle trees build with formula function
# according to Marvin only then the 0 in varIDs is the outcome
# 
#
######
require(data.tree)
require(ranger)

DEBUG = F

######################### NOTES ######################################
# Notes form Marvin from https://github.com/imbs-hl/ranger/issues/147#issuecomment-263808777
#
# child.nodeIDs is a list containing two vectors for each tree. 
# These are the child node IDs for each node in the tree. The 
# left children in the first vector, the right children in the 
# second vector. The nodeIDs are 0-indexed. However, since the 
# root of a tree (ID 0) cannot be a child, the 0 is used for 
# terminal nodes.
#
# In split.varIDs there is a vector for each tree with the 
# splitting variables for each node in the tree. These IDs 
# are 0-indexed, too. If you use the formula interface, the 
# 0-variable should be the outcome. Terminal nodes also have a 
# 0 here.
#
# In split.values the splitting values for all nodes are 
# saved. In terminal nodes, the outcome for this node ist 
# saved in split.values.
# 
# Note: A 0 in split.varIDs is no sufficient condition 
# for a terminal node. Check child.nodeIDs to be sure.
######################### NOTES END ###################################

################################################################################
################################################################################
#'
#' Function that checks if a object has the fields necessary for a ranger object
#' (only a selection of it)
#'
#' @param rangerForest a ranger forest object from the ranger package
#'
is.ranger <- function(rangerForest){
  if(is.null(rangerForest)){ #is it null?
    return(F)
  }
  if(is.null(rangerForest$forest) #checking top level fields
     || is.null(rangerForest$num.independent.variables)
     || is.null(rangerForest$predictions)
     || is.null(rangerForest$mtry)
     || is.null(rangerForest$min.node.size)
     || is.null(rangerForest$forest)){
       return(F)
  }
  
  if(is.null(rangerForest$forest$num.trees) # checking second level fields
     || is.null(rangerForest$forest$child.nodeIDs)
     || is.null(rangerForest$forest$split.varIDs)
     || is.null(rangerForest$forest$split.values)){
    return(F)    
  }
  
  return(T)
}

################################################################################
################################################################################
#'
#' Function that checks if a node is terminal. Returns T if it
#' is terminal, F if it is not terminal.
#' 
#' @param node: in case the nodeType is "term" we return true, otherwise false
#' node of data.frame with the 5 following fields:
#' $split.varIDs, $split.featureNames, $split.values, $chilsLeft, $chilsRight
#' 
#'
is.terminal <- function(node){
  #   if(node$split.featureNames=="term"){
  #     return(T) 
  #   } else{
  #     return(F)
  #   }
    if(node$childsLeft==0 || node$childsRight==0){
      if(!(node$childsLeft==0 && node$childsRight==0)){
        stop("If one child is 0, both childs have to be 0 (0 indicating a terminal node). Aborting")
      } else{ #if both childs are zero, return T
        return(T) 
      }
    } else{ #if the childs are != 0, they are not terminal
      return(F)
    }
  
}
################################################################################
################################################################################
#'
#' Function that returns T if the node with the nodename does not exist in the tree
#' stored in rootNode
#' 
#' @param rootNode a Node object from the data.tree package specifying the tree
#' @param nodeName a string containing the node name to be checked for existence
#' 
does.exist <- function(rootNode, nodeName){
  if(length(na.omit(rootNode$Get(nodeName)))>0){
    return(T)
  } else{
    return(F)
  }
}

################################################################################
################################################################################
#'
#' Function that returns the sign for either lesserequal or greater depending 
#' on the nodetype.
#'
#' @param nodetype. Right returns ">", Left returns "<="
#' 
getArrowSign <- function(nodeType){
  currSign = ""
  if(nodeType=="left"){
    currSign = "\u2264"
  } else if(nodeType=="right"){
    currSign = "\u003E"
  } else{
    # leave everything as is,
  }
  return(currSign)
}
################################################################################
################################################################################
# 
#' Function that takes a node name and checks if it is unique. If it is not unqiue
#' it adds "(n)" to the end of the string. n is the lowest number that has not been used.
#' e.g. "Petal.Width" will become in a node that tries to use the name again "Petal.Width (1)"
#' This is needed for the data.tree package (requires unique node names).
#' 
#' @param rootNode: rootNode of tree to check for names
#' @param nodeName: nodename to check
#'
getUniqueNodeName <- function(rootNode, nodeName) {
  nodeNameTmp = nodeName
  currUniquePart = ""
  while(does.exist(rootNode, nodeNameTmp)){
    #print(paste("Node with name exists:", nodeName, ". Making it unique now"))
    num = gsub("[\\(\\)]", "", regmatches(nodeNameTmp, gregexpr("\\(.*?\\)", nodeNameTmp))[[1]])
    if(length(num)>0){ #if the current string already has a number, increment it
      #print("Num is:")
      #print(num)
      num = as.numeric(num) + 1
    } else{ #otherwise set to 1
      num = 1
      #print("Num is:")
      #print(num)
    }
    currUniquePart = paste("(", num, ")", sep="")
    nodeNameTmp = paste(nodeName, currUniquePart)
  }
  return(nodeNameTmp)
}

################################################################################
################################################################################
#'
#' Function that checks if the parameters are valid (used in plot and print)
#'
#' @param rangerForest a ranger forest object
#' @param maxDepth a number
#' @param numTree a number
#' 
#' @example
#' require(ranger)
#' require(iris)
#' data(iris)
#' 
#' rangerForest = ranger(Species ~ Petal.Length + Petal.Width, data=iris)
#' maxDepth = 5
#' numTree = "-1"
#' 
#' parameterValidityCheck(rangerForest, maxDepth, numTree) #aborts if wrong type, numTree needs to be numeric
#'
parameterValidityCheck <- function(rangerForest, maxDepth, numTree){
  if(!(is.numeric(maxDepth))){
    stop(paste("maxDepth is not numeric. Aborting! (type was >", typeof(maxDepth), "<)", sep=""))
  }
  if(!(is.numeric(numTree))){
    stop(paste("numTree is not numeric. Aborting! (type was >", typeof(maxDepth), "<)", sep=""))
  }
  if(!(is.ranger(rangerForest))){
    stop("rangerForest does not seem to be a valid ranger object form the ranger package (assumption based on various tests). Aborting!")
  }
}

################################################################################
################################################################################
#'
#' Function that builds the tree recursively traversing the left childs first. Returns the
#' rootNode so it can be used later.
#' 
#' @param rootNode: typeof Node from data.tree package (to being able to search the whole tree)
#' @param currFatherNode: typeof Node from data.tree package (to add the current node to)
#' @param currIndex: the current Index in varIDs, nodeIDs and splitVals
#' @param currLevel: the current level in the tree (for enabling a stop at a certain point)
#' @param nodeType: "root", "left", "right" or "term"
#' @param maxDepth: at which depth in the tree should we stop (values between 1 and n, -1 for whole tree)
#'
#'
buildTree <- function(tree, rootNode, currFatherNode, currIndex, currLevel, nodeType, maxDepth){
  if(exists("DEBUG") && DEBUG){
    print(paste("Using currIndex:", currIndex, "| CurrLevel:", currLevel, "| NodeType:", nodeType, "| maxDepth:",maxDepth))
  }
  
  if(maxDepth != -1 && currLevel>maxDepth){
    return() # do nothing, we have reached maxDepth
  } else{
    currNodeInformation = tree[currIndex,] # get the information of the current node
    
    currVal = format(round(currNodeInformation$split.values, 2), nsmall = 2) # round to two decimals for plotting
    currFeatName = as.character(currNodeInformation$split.featureNames) #get the node name
    childLeft = currNodeInformation$childsLeft
    childRight = currNodeInformation$childsRight
    
    if(childLeft != 0){ #if it is not a terminal node, correct for index
      childLeft = childLeft + 1
    }
    if(childRight != 0){ #if it is not a terminal node, correct for index
      childRight = childRight + 1
    }
    if(exists("DEBUG") && DEBUG){
      print(paste("ChildLeft:", childLeft, "| ChildRight:", childRight))
    }
    #####
    # if it is a terminal node, plot the value in the node
    # if it is a rootNode, take only the feature name as node name and skip the 
    #    checking for other nodes with the same name (no conflict possible in root node)
    # if it is a normal node, hand it over to the function that guarantees a unqiue node name
    nodeName = ""
    if(is.terminal(currNodeInformation)){
      nodeName = paste(currIndex, ". ", currVal, sep="")
    } else{
      #nodeName = paste(paste(currIndex,".",sep=""), "\n", currFeatName, sep="")
      if(is.null(rootNode)){ #in the first call
        nodeName = currFeatName
      } else{
        nodeName = getUniqueNodeName(rootNode = rootNode, nodeName = currFeatName)
      }
    }
    
    # create new node if father is NULL (first time) or add a child with
    #   nodeName being the featurename (plus unique part if necessary),
    #   splitVal/targetVal and index corrected left and right child.
    if(is.null(currFatherNode)){
      rootNode = Node$new(nodeName, splitVal=currVal, childLeft=childLeft, childRight=childRight)
      currNode = rootNode
    } else{
      currNode = currFatherNode$AddChild(nodeName, splitVal=currVal, childLeft=childLeft, childRight=childRight)
    }
    
    if(nodeType!="root"){ #only set the arrows for the non-root nodes
      # get the correct sign for the arrow 
      currSign = getArrowSign(nodeType)
      # create label
      currLabel = paste(currSign, currFatherNode$splitVal, sep="")
      #setting the arrow type and label
      SetEdgeStyle(currNode, label = currLabel, arrowhead = "vee", color = "grey35", penwidth = 2)
    }
    
    #if it is a terminal node, set the color to light blue
    if(is.terminal(currNodeInformation)){
      #SetNodeStyle(currNode, fillcolor = "LightBlue", penwidth = "1px")
      SetNodeStyle(currNode, fillcolor = "DeepSkyBlue", penwidth = "1px")
      #colors from: http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors
    }
    
    # if we have child nodes (aka we are not terminal), call the function again
    if(!(is.terminal(node = currNodeInformation))){
      # call myself for left child
      buildTree(tree = tree,
                rootNode = rootNode,
                currFatherNode = currNode,
                currIndex = childLeft, 
                currLevel = currLevel+1,
                nodeType = "left", 
                maxDepth = maxDepth)
      
      # call myself for right child
      buildTree(tree = tree,
                rootNode = rootNode,
                currFatherNode = currNode,
                currIndex = childRight, 
                currLevel = currLevel+1,
                nodeType = "right", 
                maxDepth = maxDepth)
    }
  }
  return(rootNode)
}

################################################################################
################################################################################
#'
#' Function that generates the tree based on a forest and optionally 
#' maxDepth (how many levels until stop) and numTree (which tree to plot)
#'
#' @param rangerForest a rangerForest object
#' @param maxDepth a number specifying the maximum depth of the tree to be build
#' @parma numTree a number specifying which tree from the forest to plot
#' 
generateTree <- function(rangerForest, maxDepth, numTree){
  parameterValidityCheck(rangerForest=rangerForest, maxDepth=maxDepth, numTree=numTree)
  
  #getting predictor names from forest
  predictorNames = c("outcome",rangerForest$forest$independent.variable.names)
  
  featNames = predictorNames[(rangerForest$forest$split.varIDs[numTree][[1]]+1)] # incrementing the varIDs by one, so that it fits with the above predictors array
  
  #building a more readable structure of the tree 
  tree = data.frame(split.varIDs=rangerForest$forest$split.varIDs[[numTree]],
                    split.featureNames=featNames,
                    split.values=rangerForest$forest$split.values[[numTree]],
                    childsLeft=rangerForest$forest$child.nodeIDs[[numTree]][1][[1]],
                    childsRight=rangerForest$forest$child.nodeIDs[[numTree]][2][[1]])
  
  #building the tree recursively and saving the rootNode
  rootNode = buildTree(tree = tree,
                       rootNode = NULL,
                       currFatherNode = NULL,
                       currIndex = 1, 
                       currLevel = 1, 
                       nodeType="root", 
                       maxDepth = maxDepth)
  
  return(rootNode)
}

################################################################################
################################################################################
#'
#' Function that plots a ranger forest.
#' 
#' @param rangerForest a ranger forest object (from package ranger)
#' @param maxDepth a number describing the maximum depth the tree should be plotted
#' @param numTree a number specifying which tree to plot
#' 
#' @example
#' require(ranger)
#' require(iris)
#' data(iris)
#' 
#' rangerForest = ranger(Species ~ Petal.Length + Petal.Width, data=iris)
#' 
#' plotRanger(rangerForest, 5, 1)
#' 
plotRanger <- function(rangerForest, maxDepth = 5, numTree=1){
  parameterValidityCheck(rangerForest=rangerForest, maxDepth=maxDepth, numTree=numTree)
  
  rootNode = generateTree(rangerForest = rangerForest, maxDepth = maxDepth, numTree = numTree)
  
  #seeting some graphical options
  #  nodes are grey,
  #  leaves are light blue
  #
  #SetGraphStyle(rootNode, rankdir = "TB")
  SetEdgeStyle(rootNode, arrowhead = "vee", color = "grey35", penwidth = 2)
  SetNodeStyle(rootNode, style = "filled,rounded", shape = "box", fillcolor = "lightgrey", fontname = "helvetica", tooltip = GetDefaultTooltip)

   #maybe i find out at some point how to redirect to file (the normal way via png does not work) 
  #png(filename="/tmp/bla.png")
  plot(rootNode)
  #dev.off()
  return(rootNode)
}

################################################################################
################################################################################
#'
#'
#' Function that prints the tree structure in text form.
#'
#' @param rangerForest a forest learned with ranger
#' @param maxDepth a number specifying the maximum depth of the tree build
#' @param numTree a number specifying the tree to print
#' 
#' @example
#' require(ranger)
#' require(iris)
#' data(iris)
#' 
#' rangerForest = ranger(Species ~ Petal.Length + Petal.Width, data=iris)
#' 
#' printRanger(rangerForest)
#' printRanger(rangerForest, 15, 1)
#' 
###
printRanger <- function(rangerForest, maxDepth = 5, numTree = 1){
  parameterValidityCheck(rangerForest=rangerForest, maxDepth=maxDepth, numTree=numTree)
  
  rootNode = generateTree(rangerForest = rangerForest, maxDepth = maxDepth, numTree = numTree)
  # print the forest together with the additional information we put into the tree
  print(rootNode, "splitVal", "childLeft", "childRight")
  return(rootNode)
}

############################################################################
############################################################################
################### EXAMPLES EXAMPLES EXAMPLES #############################
############################################################################
############################################################################
############################################################################


require(ranger)
data(iris)
fmlaTarget = "Species"
fmlaPredictors = c("Petal.Length", "Petal.Width")
fmla = as.formula(paste(fmlaTarget, paste(fmlaPredictors,collapse=" + "), sep=" ~ "))
forest = ranger(fmla, data=iris,num.trees = 2)

# DEBUG: Save for later use (and to be sure it is always the same tree):
#save(x=forest,file="/tmp/bla.ranger.RData")
#load("/tmp/bla.ranger.RData") #load to always have the same tree

#plotting the tree, unfortunately does this not work yet, so this step has to
#be executed manually and can not be called from within a script or by rstudio "Source on Save"
rootNode = plotRanger(forest, maxDepth = -1, numTree = 2)
plot(rootNode)

printRanger(forest, maxDepth=-1, numTree=2)

