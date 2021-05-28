# user structure
user <- 
  list(
    sessionData = 
      list(
          county_chosen = character(0),
          filtered_data = vector("list",0),
          game = 
              list(
                 puzzle_guess = vector("list",0)
              )
      ),
    checkIns = vector("list",0),
    visits = vector("list",0)
  )
# heritage structure
heritage <-
  list(
    data = heritageData,
    show_county = function(){},
    filter = list(
      countyChosen = function(county){}
    ),
    game = list(
      puzzle_guess = function(){}
    ),
    validate_checkIn = function(returnedGPS){}
    track = function(list_tracks){}
  )



