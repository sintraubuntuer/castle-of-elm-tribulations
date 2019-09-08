# Castle Of Elm Tribulations 

Castle of Elm Tribulations is a minimalistic Rogue like game , inspired by [Claudia Doppioslash](https://github.com/doppioslash/CastleOfElm)  Castle of Elm  , Atic Atac , Roguelike in Elm by [Alex Neslusan](https://github.com/deadfoxygrandpa/Roguelike) , Sleeping Beauty's Game of Thorns by [Ondřej Žára](https://github.com/ondras/sleeping-beauty) , ... 

It uses art from the original Castle of Elm game , by  http://lpc.opengameart.org/static/lpc-style-guide/assets.html and  by 
Pedro Medeiros (@saint11)  ...

The game is already functional , and i strongly advise you to play it :-)  , but it can still be improved in some ways ... like for instance by adding sound ( if you want to collaborate please let me know )

The project ( on my part ) started as an attempt to convert the C# RogueSharp library ( https://github.com/FaronBracy/RogueSharp ) to Elm and that's what most of  MapGen module is all about ( although some algorithms  are still  missing ) ...

... and this year i suddenly decided to also use it to make a nice little game for LOWREZJAM 2019 
( a game with just around 600kb and that only pushes CPU temps to around 60º  :D  )


There's still a lot of code cleanup to do but i decided to share it anyway 


Game Hints :
Find your way through the Caverns, Basement , The Ground Floor , First Floor and the Attic , pick up the three pieces of paper with the codes to unlock the striped door and move towards enlightenment !!!

Use Q, A , O , P ( or Arrow keys) to move  

U to pick up items  

I for inventory  

S for Stats  

E for Opponent Report

F to turn fog on and off

M for map ( use with caution - algo is still being optimized - right now it takes around 3 secs to render the map )

and H for Help"


### Grid-3d 
I tried to use a 3d Grid , which makes the code cleaner and nicer ( and added a branch for that ) , 
instead of using a 2d-grid that gets replaced  when the player changes floors ,
but ended up deciding not to use it , because it makes the game run slower .
Maybe someone can help in optimizing the code ... ( not sure if that's possible )
The branch is 3d-Grid-try-version


## Instructions to run

1. Install elm [elm-lang.org/install](http://elm-lang.org/install)
2. Clone this repo and `cd` into it
3. Run `elm make src/Main.elm --output ./dist/castleOfElmTribulations.js`
4. Open `index.html` in the browser



