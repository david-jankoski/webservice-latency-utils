const loadtest = require('C:/Program Files/nodejs/node_modules/loadtest');
const fs = require('fs');

const lookres = fs.readFileSync("lookup_result.json", "utf8");

const maxreq = +process.argv[2],
	  conc = +process.argv[3];

console.log("Starting loadtest with maxreq=%j and conc=%j", maxreq, conc);

var testres = [];


function statusCallback(error, result, latency) {
    
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
    
    url: 'http://192.168.1.207/ocpu/library/hellotripv2/R/predict/json',
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
    if (error)
    {
        return console.error('Got an error: %s', error);
    }
    console.log('Tests run successfully');


    console.log('-------------');
    console.log('Writing out results...');

    fs.writeFileSync(
    	"latency_test_results/chng_conf/latency_mr" + maxreq + "_con" + conc + "_results.json", 
    	JSON.stringify(testres), "utf8");

});
