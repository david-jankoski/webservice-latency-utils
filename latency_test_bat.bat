@echo off

set maxreqs=500
set concs=3 4 5 6 7 8 9 10 11 12

FOR %%A IN (%maxreqs%) DO (

	FOR %%B IN (%concs%) DO (
		
		node latency_test_js.js %%A %%B

	)
	
)