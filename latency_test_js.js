// Load modules ----------

// Note - assumes you did 'npm install loadtest' in working dir of this project
const loadtest = require("loadtest");
const fs = require("fs"); 

// Check if sample results file exists, read in
fs.access("lookup_result.json", err => console.log(err));
const lookres = fs.readFileSync("lookup_result.json", "utf8");

// TODO make this arg
const results_dir = "latency_analysis/results/";

// Parse cmd line args
const maxreq = +process.argv[2],
	  conc = +process.argv[3],
	  testurl = process.argv[4];

//  Construct output path to store results

// time now in format YYYYmmddHHMMSS
const time_now = new Date().toISOString().replace(/-|T|:/g, "").split(".")[0];
	  
	  // extract sub test url (smth like hellotripv2-demo)
	  //
	  // note - inital idea was to extract the domain substring from provided
	  // testurl, however many possible varous combinations of this led to
	  // some problems therefore this line is commented out till i figure out
	  // a more robust way of doing this - or meet a regex guru (anyone?)
	  // sub_testurl = testurl.replace(/^http(?:s)?\:\/\/(?:www\.)?/i, "").split(".")[0],
	  //
	  // therefore the workaround checks if testurl is on localhost or smth else
const sub_testurl = 
	  		testurl.includes("localhost") ? "local" : 
	  		testurl.replace(/^http(?:s)?\:\/\/(?:www\.)?/i, "").split(".")[0];

	  // format : [system time]_[test url]_[max reqs]_[conc].json
	  // example: 20180404151552_website-demo_555_7.json      
const results_path = 
		results_dir +
      	time_now + "_" + 
        sub_testurl + "_" + 
        maxreq + "_" + 
        conc + 
        ".json";

console.log("\n...Starting loadtest on \n\turl='%s' \n\tmaxreq=%j \n\tconc=%j \n\tresults_path='%s' \n", testurl, maxreq, conc, results_path);

var testres = [];


function statusCallback(error, result, latency) {
    
    // Useful things for debugging when smth goes wrong
    //console.log('Current latency %j, result %j, error %j', latency, result, error);
    //console.log('----');
    //console.log('Request elapsed milliseconds: ', result.requestElapsed);
    //console.log('Request index: ', result.requestIndex);
    //console.log('Request loadtest() instance index: ', result.instanceIndex);
	//console.log('Current reqInd %j, latency %j', result.requestIndex, latency);
	
    if (error) {
    	return console.error('Got an error: %s', error);
    }

    console.log("Done with request idx:%j", result.requestIndex);

    latency.requestIndex = result.requestIndex;
	//console.log(latency);
	testres.push(latency);
	//console.log(testres);
}

var options = {
    
    url: testurl,
    method: 'POST',
    contentType: 'application/json',
    agentKeepAlive: 'true',
    
    maxRequests: maxreq,
    concurrency: conc,
    
    body: {
    	lookup_result: lookres
    },    
    statusCallback: statusCallback
};

loadtest.loadTest(options, function(error, result)
{
    if (error) return console.error('Got an error: %s', error);

    console.log("Tests run successfully! Writing out results to %s", results_path);
    
    fs.writeFileSync(results_path, JSON.stringify(testres), "utf8");

});
