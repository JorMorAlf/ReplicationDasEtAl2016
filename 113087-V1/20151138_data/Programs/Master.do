/*

Master file: Calls all do-files used in the analysis of the paper in order and produces outputs

*/
set more off
set matsize 3000
cd "C:\Users\amohpal\Desktop\AER Submission\20151138_data" // USER ACTION REQUIRED TO CHANGE DIRECTORY

* Install user-written programs
	* ssc install xml_tab.ado
	* ssc install winsor
	* install "pvreg.do" by copying it in the ado-folder

* Main Tables
do "Programs\Table1.do"
do "Programs\Table2.do"
do "Programs\Table3.do"
do "Programs\Table4.do"
do "Programs\Table5.do"
do "Programs\Table6.do"
do "Programs\Table7.do"
do "Programs\Table8.do"

* Appendix Tables
do "Programs\TableA3.do"
do "Programs\TableA4.do"
do "Programs\TableA6.do"
do "Programs\TableA7.do"
do "Programs\TableA8.do"
do "Programs\TableA9.do"
do "Programs\TableA10.do"
do "Programs\TableA11.do"
do "Programs\TableA12.do"
do "Programs\TableA13.do"
do "Programs\TableA14.do"
do "Programs\TableA15.do"
do "Programs\TableA16.do"
do "Programs\TableA17.do"
do "Programs\TableA18.do"
do "Programs\TableA19.do"

