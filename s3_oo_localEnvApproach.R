NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{

      ## Get the environment for this
      ## instance of the function.
      thisEnv <- environment()

      hasBreakfast <- eatsBreakfast
      favoriteBreakfast <- myFavorite

      ## Create the list used to represent an
      ## object for this class
      me <- list(

        ## Define the environment where this list is defined so
        ## that I can refer to it later.
        thisEnv = thisEnv,

        ## The Methods for this class normally go here but are discussed
        ## below. A simple placeholder is here to give you a teaser....
        getEnv = function()
        {
              return(get("thisEnv",thisEnv))
        }

        )

      ## Define the value of the list within the current environment.
      assign('this', me, envir=thisEnv)
      ## assign(x, value, env) , make the hashmap map$x = value

      ## Set the name for the class
      class(me) <- append(class(me),"NordAmericain")
      return(me)
}


bubba <- NordAmericain()
get("hasBreakfast",bubba$getEnv())
get("favoriteBreakfast",bubba$getEnv())
# [1] "cereal"
bubba <- NordAmericain(myFavorite="oatmeal")
get("favoriteBreakfast",bubba$getEnv())
louise <- bubba
assign("favoriteBreakfast","toast",louise$getEnv())
get("favoriteBreakfast",louise$getEnv())
get("favoriteBreakfast",bubba$getEnv())

####### object is a hash map
NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{

      ## Get the environment for this
      ## instance of the function.
      thisEnv <- environment()

      hasBreakfast <- eatsBreakfast
      favoriteBreakfast <- myFavorite

      ## Create the list used to represent an
      ## object for this class
      me <- list(

              ## Define the environment where this list is defined so
              ## that I can refer to it later.
              thisEnv = thisEnv,

              ## Define the accessors for the data fields.
              getEnv = function()
              {
                      return(get("thisEnv",thisEnv))
              },

              getHasBreakfast = function()
              {
                      return(get("hasBreakfast",thisEnv))
              },

              setHasBreakfast = function(value)
              {
                      return(assign("hasBreakfast",value,thisEnv))
              },


              getFavoriteBreakfast = function()
              {
                      return(get("favoriteBreakfast",thisEnv))
              },

              setFavoriteBreakfast = function(value)
              {
                      return(assign("favoriteBreakfast",value,thisEnv))
              }

        )

      ## Define the value of the list within the current environment.
      assign('this',me,envir=thisEnv)

      ## Set the name for the class
      ## me is a list containing all the fields, which contains the environment-- the hash map itself
      class(me) <- append(class(me),"NordAmericain")
      return(me)
}

## now it looks like more OO
bubba <- NordAmericain(myFavorite="oatmeal")
bubba$getFavoriteBreakfast()
bubba$setFavoriteBreakfast("plain toast")
bubba$getFavoriteBreakfast()

makeCopy <- function(elObjeto)  ## copy constructor is outside the scope
        {
                print("Calling the base makeCopy function")
                UseMethod("makeCopy",elObjeto)
                print("Note this is not executed!")
        }

makeCopy.default <- function(elObjeto)
        {
                print("You screwed up. I do not know how to handle this object.")
                return(elObjeto)
        }


makeCopy.NordAmericain <- function(elObjeto)
        {
                print("In makeCopy.NordAmericain and making a copy")
                newObject <- NordAmericain(
                        eatsBreakfast=elObjeto$getHasBreakfast(),
                        myFavorite=elObjeto$getFavoriteBreakfast())
                return(newObject)
        }
