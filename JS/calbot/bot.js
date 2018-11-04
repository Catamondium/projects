#!/usr/bin/env node

const fs = require('fs');
const readline = require('readline');
const {google} = require('googleapis'); // {} cast to object
const parse = require('./parser.js').parse;

// If modifying these scopes, delete token.json.
const SCOPES = [
"https://www.googleapis.com/auth/calendar.readonly",
"https://www.googleapis.com/auth/calendar.events"];
const TOKEN_PATH = 'token.json';

// Load client secrets from a local file.
fs.readFile('credentials.json', (err, content) => {
	if (err) return console.log('Error loading client secret file:', err);
  // Authorize a client with credentials, then call the Google Calendar API.
	authorize(JSON.parse(content), main);
});

/**
 * Create an OAuth2 client with the given credentials, and then execute the
 * given callback function.
 * @param {Object} credentials The authorization client credentials.
 * @param {function} callback The callback to call with the authorized client.
 */
function authorize(credentials, callback) {
	const {client_secret, client_id, redirect_uris} = credentials.installed;
	const oAuth2Client = new google.auth.OAuth2(
		client_id, client_secret, redirect_uris[0]);
	
	// Check if we have previously stored a token.
	fs.readFile(TOKEN_PATH, (err, token) => {
		if(err) return getAccessToken(oAuth2Client, callback);
		oAuth2Client.setCredentials(JSON.parse(token));
		callback(oAuth2Client);
	});
}

/**
 * Get and store new token after prompting for user authorization, and then
 * execute the given callback with the authorized OAuth2 client.
 * @param {google.auth.OAuth2} oAuth2Client The OAuth2 client to get token for.
 * @param {getEventsCallback} callback The callback for the authorized client.
 */
function getAccessToken(oAuth2Client, callback) {
	const authUrl = oAuth2Client.generateAuthUrl({
		access_type: 'offline',
		scope: SCOPES,
	});
	console.log('Authorize this app by visiting this url:', authUrl);
	const rl = readline.createInterface({
		input: process.stdin,
		output: process.stdout,
	});
	rl.question('Enter the code from that page here: ', (code) => {
		rl.close();
		oAuth2Client.getToken(code, (err, token) => {
			if (err) return console.error('Error retrieving access token', err);
			oAuth2Client.setCredentials(token);
			// Store the token to disk for later program executions
			fs.writeFile(TOKEN_PATH, JSON.stringify(token), (err) => {
				if (err) console.error(err);
				console.log('Token stored to', TOKEN_PATH);
			});
			callback(oAuth2Client);
		});
	});
}

function logCals(api) {
	api.calendarList.list({showHidden : true},
		(err, cals) => { // Never called
			if (err) throw err;
			sums = cals.data.items.map(x => x = x.summary);
			console.log("Calendars:");
			sums.forEach(item => {
				console.log(`\t${item}`);
			});
		}
	);
}

function simplify(item) {
	ret = {sum: item.summary,
		rec: item.recurrence,
		id: item.id};
	return ret;
}

function delEvents(api, name, data) {
	api.calendarList.list({showHidden : true},
		(err, cals) => {
			if (err) throw err;
			calIDs = cals.data.items.map(x => x = simplify(x));
			let cal;
			
			calIDs.some(item => {
				if (item.sum == name) {
					cal = item.id;
					return true
				}});
			
			if (!cal) {
				console.log("Calendar not found.");
				logCals(api);
				process.exit(1);
			} else
				getEvents(api, cal, data);
		}
	);
}

function getEvents(api, cal, data) {
	data.forEach(hol => {
		api.events.list({
			calendarId: cal,
			timeMin: hol.start,
			timeMax: hol.end},
			(err, events) => {
				if (err) throw err;
				eventIDs =  new Set(events.data.items.map(
					x => x = simplify(x)));
				eventIDs.forEach(ev => {
					if (ev.rec) {
						expand(api, cal, ev, hol)
					}
				})}
		)})
}

function expand(api, cal, ev, hol) {
	api.events.instances({
		calendarId: cal,
		eventId: ev.id,
		timeMin: hol.start,
		timeMax: hol.end},
		(err, events) => {
			if (err) throw err;
			eventIDs = events.data.items.map(
				x => x = x.id);
			del(api, cal, eventIDs);
		});
}

function del(api, cal, events) {
	events.forEach(ev => {
		api.events.delete({
			calendarId: cal,
			eventId: ev});
	});
}

function main(auth) {
	const calendar = google.calendar({version: 'v3', auth});
	let args = process.argv.slice(2);
	if (args.length == 0) {
		console.log("Usage: ./bot.js spec calendar");
		process.exit(1);
	} else if (args.length == 1) {
		logCals(calendar);
		process.exit(1);
	}  else {
		let dat = parse(args[0]);
		delEvents(calendar, args[1], dat);
	}
}
