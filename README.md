a interface for serving [mbtiles](https://github.com/mapbox/mbtiles-spec) in erlang

you should be able to install with make, i.e.

	make
	
you may have to first do

	sudo apt-get install erlang sqlite3
	
at the moment it assumes that the tiles are in the folder called tiles, there is one in there for testing called gc (it's massachusetts governor's council districts). To run from this directory in the erl prompt.

	mbtiles:start(gc).

to start it then to get a tile

	mbtiles:get(gc,tile,8,77,94).
	
should get <<numbers>> to see it try 

	file:write_file("94.png",mbtiles:get(gc,tile,8,77,94)). 
	
should see a tile. Try

	mbtiles:get(gc,grid,8,77,94).
	
and

	file:write_file("94.grid.json",mbtiles:get(gc,grid,8,77,94)).
	
for grid json 
