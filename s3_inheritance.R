NorthAmerican <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{

  me <- list(
    hasBreakfast = eatsBreakfast,
    favoriteBreakfast = myFavorite
 )

  ## Set the name for the class
  class(me) <- append(class(me),"NorthAmerican")
  return(me)
}



setHasBreakfast <- function(elObjeto, newValue)
    {
        UseMethod("setHasBreakfast",elObjeto)
        print("Note this is not executed!")
    }

setHasBreakfast.default <- function(elObjeto, newValue)
    {
        print("You screwed up. I do not know how to handle this object.")
        return(elObjeto)
    }


setHasBreakfast.NorthAmerican <- function(elObjeto, newValue)
    {
        elObjeto$hasBreakfast <- newValue
        return(elObjeto)
    }


getHasBreakfast <- function(elObjeto)
    {
        UseMethod("getHasBreakfast",elObjeto)
        print("Note this is not executed!")
    }

getHasBreakfast.default <- function(elObjeto)
    {
        print("You screwed up. I do not know how to handle this object.")
        return(NULL)
    }


getHasBreakfast.NorthAmerican <- function(elObjeto)
    {
        return(elObjeto$hasBreakfast)
    }


setFavoriteBreakfast <- function(elObjeto, newValue)
    {
        UseMethod("setFavoriteBreakfast",elObjeto)
        print("Note this is not executed!")
    }

setFavoriteBreakfast.default <- function(elObjeto, newValue)
    {
        print("You screwed up. I do not know how to handle this object.")
        return(elObjeto)
    }


setFavoriteBreakfast.NorthAmerican <- function(elObjeto, newValue)
    {
        elObjeto$favoriteBreakfast <- newValue
        return(elObjeto)
    }


getFavoriteBreakfast <- function(elObjeto)
    {
        UseMethod("getFavoriteBreakfast",elObjeto)
    }

getFavoriteBreakfast.default <- function(elObjeto)
    {
        print("You screwed up. I do not know how to handle this object.")
        return(NULL)
    }


getFavoriteBreakfast.NorthAmerican <- function(elObjeto)
    {
        return(elObjeto$favoriteBreakfast)
    }

bubba <- NorthAmerican()
bubba <- setHasBreakfast(bubba,FALSE)
bubba <- setFavoriteBreakfast(bubba,"Pork Belly")
getHasBreakfast(bubba)
getFavoriteBreakfast(bubba)


#####################################
## Now define the derived classes
Mexican <- function(eatsBreakfast=TRUE,myFavorite="los huevos")
{

  me <- NorthAmerican(eatsBreakfast,myFavorite)

  ## Add the name for the class
  class(me) <- append(class(me),"Mexican")
  return(me)
}


USAsian <- function(eatsBreakfast=TRUE,myFavorite="pork belly")
{

  me <- NorthAmerican(eatsBreakfast,myFavorite)

  ## Add the name for the class
  class(me) <- append(class(me),"USAsian")
  return(me)
}

Canadian <- function(eatsBreakfast=TRUE,myFavorite="back bacon")
{

  me <- NorthAmerican(eatsBreakfast,myFavorite)

  ## Add the name for the class
  class(me) <- append(class(me),"Canadian")
  return(me)
}

Anglophone <- function(eatsBreakfast=TRUE,myFavorite="pancakes")
{

  me <- Canadian(eatsBreakfast,myFavorite)

  ## Add the name for the class
  class(me) <- append(class(me),"Anglophone")
  return(me)
}

Francophone <- function(eatsBreakfast=TRUE,myFavorite="crepes")
{

  me <- Canadian(eatsBreakfast,myFavorite)

  ## Add the name for the class
  class(me) <- append(class(me),"Francophone")
  return(me)
}

makeBreakfast <- function(theObject)
  {
    print("Calling the base makeBreakfast function")
    UseMethod("makeBreakfast",theObject)
  }

makeBreakfast.default <- function(theObject)
  {
    print(noquote(paste("Well, this is awkward. Just make",
                        getFavoriteBreakfast(theObject))))
    return(theObject)
  }

makeBreakfast.Mexican <- function(theObject)
  {
    print(noquote(paste("Estoy cocinando",
                        getFavoriteBreakfast(theObject))))
    NextMethod("makeBreakfast",theObject)
    return(theObject)
  }

makeBreakfast.USAsian <- function(theObject)
  {
    print(noquote(paste("Leave me alone I am making",
                        getFavoriteBreakfast(theObject))))
    NextMethod("makeBreakfast",theObject)
    return(theObject)
  }

makeBreakfast.Canadian <- function(theObject)
  {
    print(noquote(paste("Good morning, how would you like",
                        getFavoriteBreakfast(theObject))))
    NextMethod("makeBreakfast",theObject)
    return(theObject)
  }

makeBreakfast.Anglophone <- function(theObject)
  {
    print(noquote(paste("I hope it is okay that I am making",
                        getFavoriteBreakfast(theObject))))
    NextMethod("makeBreakfast",theObject)
    return(theObject)
  }

makeBreakfast.Francophone <- function(theObject)
  {
    print(noquote(paste("Je cuisine",
                        getFavoriteBreakfast(theObject))))
    NextMethod("makeBreakfast",theObject)
    return(theObject)
  }


francois <- Francophone()
francois
class(francois)
francois <- makeBreakfast(francois)

