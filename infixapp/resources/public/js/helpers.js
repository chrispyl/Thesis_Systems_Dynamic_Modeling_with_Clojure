var resp;
var currentIter;
var objkeys;
var iterations;
var chart=null;


function hideNavigationButtons(){
	document.getElementById('previousButton').style.visibility = 'hidden';
	document.getElementById('nextButton').style.visibility = 'hidden';
}

function showNavigationButtons(){
	document.getElementById('previousButton').style.visibility = 'visible';
	document.getElementById('nextButton').style.visibility = 'visible';
}

function range(start, end, step) {
	var range = [];
	var typeofStart = typeof start;
	var typeofEnd = typeof end;

	if (step === 0) {
		throw TypeError("Step cannot be zero.");
	}

	if (typeofStart == "undefined" || typeofEnd == "undefined") {
		throw TypeError("Must pass start and end arguments.");
	} else if (typeofStart != typeofEnd) {
		throw TypeError("Start and end arguments must be of same type.");
	}

	typeof step == "undefined" && (step = 1);

	if (end < start) {
		step = -step;
	}

	if (typeofStart == "number") {

		while (step > 0 ? end >= start : end <= start) {
			range.push(start);
			start += step;
		}

	} else if (typeofStart == "string") {

		if (start.length != 1 || end.length != 1) {
			throw TypeError("Only strings with one character are supported.");
		}

		start = start.charCodeAt(0);
		end = end.charCodeAt(0);

		while (step > 0 ? end >= start : end <= start) {
			range.push(String.fromCharCode(start));
			start += step;
		}

	} else {
		throw TypeError("Only string and number types are supported");
	}

	return range;
}

function processInput(){
 
	var files = document.getElementById('files').files;
	var results = [];
	
	//console.log(files.length);
	//console.log(typeof files[0].name == 'string'); //means it's file
	//console.log(typeof files[1].name == 'string');	
	
	if(files.length>0){
		for (var i=0; i<files.length; i++) {			
			var reader = new FileReader();
			var fileText;
			reader.onload = function(evt) {
				fileText=evt.target.result;
				var parseResult = Papa.parse(fileText, {dynamicTyping: true}); //returns a parse results object
				//console.log(JSON.stringify(parseResult));
				results.push(parseResult); //array of objects
				if(results.length === files.length){
					
					//console.log(JSON.stringify(results[0]));
					//console.log("results lenght is " + results.length);
					
					//all files have been read				
					var finalCollumnNames = [];
					var finalValues = [];
					
					for(var resultsIter=0; resultsIter<results.length; resultsIter++){		
						var collumnNames = results[resultsIter].data[0];
						//console.log("collumnNames are " + collumnNames);
						
						var values = [];
						for(var i=0; i<collumnNames.length; i++) values.push([]);			
						
						for(var i=1, rows=results[resultsIter].data.length; i<rows; i++){
							for(var j=0; j<collumnNames.length; j++){
								//console.log("i " + i + " j " + j);
								values[j].push(results[resultsIter].data[i][j]);
							}
						}
						
						finalCollumnNames = finalCollumnNames.concat(collumnNames);
						finalValues = finalValues.concat(values);
					}
					
					//console.log(finalCollumnNames);
					//console.log(finalValues);
					
					var jsonObj = {};
					//console.log("finalCollumn names length " + finalCollumnNames.length);
					for(var i=0; i<finalCollumnNames.length; i++) jsonObj[finalCollumnNames[i]] = finalValues[i]; //ads the specified fields with values finalvalues to json obj 
					jsonObj = {fileValues: jsonObj};
					//console.log(JSON.stringify(jsonObj));
					
					//all the rest stuff here
					postData(jsonObj);
					
					/////////////////////////
				}
			};
			reader.readAsText(files[i], "UTF-8");
		}
	}else{
		postData({fileValues: {}});
	}	
}
			
function drawChart(){
	var key = objkeys[currentIter];	
	var ctx = document.getElementById("myChart").getContext("2d");
	chart && chart.destroy(); //to delete the previous instance, to prevent from changing graphs while hovering
	chart = new Chart(ctx, {
		type: 'line',
		data: {
			labels: iterations,
			datasets: [{
				label: key,
				fill: false,
				cubicInterpolationMode: "monotone",
				borderColor: "rgba(151,187,205,1)",
				backgroundColor: "rgba(151,187,205,1)",
				data:  resp[key]
			}]
		},
		options: {
		  
		}
	});
}


function next() {
	if(currentIter == (objkeys.length - 1)){ 
		currentIter = 0;
	} else {
		currentIter++;
	}				
}

function previous() {
	if(currentIter == 0){
		currentIter = objkeys.length - 1;
	} else {
		currentIter--;
	}				
}

function nextGraph()
{
	next();
	drawChart();
}

function previousGraph()
{
	previous();
	drawChart();
}

function initGlobals(start, end, step) {
	iterations = range(start, end, step);
	currentIter=0;
	objkeys=[];
	resp=null;
}

function constructJSON(strs, start, end, step, jsonObjFromFiles) {	
	var jsonObj = {strings: strs, start: Number(start), end: Number(end), step: Number(step)};
	var mergedJSONs = Object.assign(jsonObj, jsonObjFromFiles);
	//console.log(mergedJSONs);
	//console.log(JSON.stringify(mergedJSONs));
	return JSON.stringify(mergedJSONs);
}

function postJSONMessage (msg) {
	var initialTextareaVal = document.getElementById('textArea').value;
	var request = new XMLHttpRequest();
	request.open("POST", "/", true);
	request.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
	request.onreadystatechange = function() {
		if (request.readyState === 4 && request.status === 200) {
			document.getElementById('textArea').value = initialTextareaVal;
			resp=JSON.parse(request.responseText);
			for(var k in resp) objkeys.push(k);
			showNavigationButtons();
			drawChart();
		} else {
			document.getElementById('textArea').value = "Calculating, please wait...";
		}
	};
	request.send(msg);
}

function postData(jsonObjFromFiles) {
	var textAreaContent = document.getElementById("textArea").value;
	var strings = textAreaContent.split("\n"); //it's an array
	var start = document.getElementById("start").value;
	var end = document.getElementById("end").value;
	var step = document.getElementById("step").value;
	initGlobals(Number(start), Number(end), Number(step)); //they are strings till now
	
	postJSONMessage(constructJSON(strings, start, end, step, jsonObjFromFiles));
}