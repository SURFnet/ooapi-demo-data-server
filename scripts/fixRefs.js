const fs = require('fs');
const f_in = "./ooapiv5-refs.json";
const f_out = "./ooapiv5-norefs.json";

// read file in
let json = require(f_in);

// this is the content of a ref
function getContent(ref) {
	let parts = ref.substr(2).split("/");
	let content = json[parts[0]][parts[1]][parts[2]];
	console.log("  content: " + JSON.stringify(content));

	return content;
}

// traverse json.paths
// look for: json.paths.get.parameters.$ref in parameters
// look for: json.paths.get.parameters.schema.$ref in 'query' parameters
let count = 0;
Object.entries(json.paths).forEach(([key,value]) => {
	if ((value.get != undefined) && (value.get.parameters != undefined)) {
		// this path has parameters
		console.log("path: " + key);

		value.get.parameters.forEach((item, index) => {
			if (item.hasOwnProperty("$ref")) {
				// this parameter is ref
				console.log("  ref: " + item.$ref);

				// fixIt
				value.get.parameters[index] = getContent(item.$ref);

				// count one...
				count++;

			}
			else if (item.in === "query") {
				if (item.schema.hasOwnProperty("$ref")) {
					// this parameter has schema with ref
					console.log("  param: " + item.name);
					console.log("  ref: " + item.schema.$ref);

					// fixIt
					value.get.parameters[index].schema = getContent(item.schema.$ref);

					// count one...
					count++;
				}
			}
		});
	}
})

// write to file out
fs.writeFile(f_out, JSON.stringify(json, null, 2), err => {} );

console.log("\n---\nnumber of refs replaced with content in parameters: " + count);
