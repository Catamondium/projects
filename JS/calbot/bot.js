#!/usr/bin/env node
const fs = require('fs');
const readline = require('readline');
const { google } = require('googleapis');
const { parse } = require('./parser.js');
const { promisify } = require('util');
const { argv } = require('process');

//****  AUTH
const credentials = require('./credentials.json');

// If modifying these scopes, delete token.json.
const SCOPES = [
    "https://www.googleapis.com/auth/calendar.readonly",
    "https://www.googleapis.com/auth/calendar.events"];
const TOKEN_PATH = 'token.json';

/**
 * Create an OAuth2 client with the given credentials, and then execute the
 * given callback function.
 * @param {Object} credentials The authorization client credentials.
 * @param {function} callback The callback to call with the authorized client.
 */
function authorize(callback) {
    const {client_secret, client_id, redirect_uris } = credentials.installed;
    const oAuth2Client = new google.auth.OAuth2(
        client_id, client_secret, redirect_uris[0]);

    // Check if we have previously stored a token.
    fs.readFile(TOKEN_PATH, (err, token) => {
        if (err) return getAccessToken(oAuth2Client, callback);
        oAuth2Client.setCredentials(JSON.parse(token));
        callback(oAuth2Client).catch(err => {
            console.error(err);
        });
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
            fs.writeFileSync(TOKEN_PATH, JSON.stringify(token));
            callback(oAuth2Client).catch(err => {
                console.error(err);
            });
        });
    });
}
//**** END OF AUTH

// print available calendars
function logCals(api) {
    api.calendarList.list({ showHidden: true, minAccessRole: "writer" },
        (err, cals) => {
            if (err) throw err;
            sums = cals.data.items.map(x => x = x.summary);
            console.log("Calendars:");
            sums.forEach(item => {
                console.log(`\t${item}`);
            });
        }
    );
}

async function get_hols(api, name, data) {
    cals = await promisify(api.calendarList.list)({ showHidden: true, minAccessRole: "writer" });
    calIDs = cals.data.items;

    cal = calIDs.find(item => item.summary == name)

    if (!cal) {
        logCals(api);
        throw `Calendar ${name} not found`;
    }
        
    return await getEvents(api, cal.id, data);
}

function expand(api, params, events) {
    ret = new Array();
    events.filter(ev => ev.recurrence !== undefined).forEach(ev => {
        // refused promisify??
        api.events.instances({eventId: ev.id, ...params}, (err, res) => {
            if (err) throw err;
            ret.push(res.data.items.map(x => x.id)); // race condition?
        })
    });
    return ret;
}

async function getEvents(api, cal, data) {
    lst = promisify(api.events.list);
    ids = data.map(async function (hol) {
        params = {
            calendarId: cal,
            timeMin: hol.start,
            timeMax: hol.end
        };
        events = await lst(params);
        eventIDs = Array.from(new Set(events.data.items));
        return expand(api, params, eventIDs);
    });
    return await Promise.all(ids);
}

function del(api, cal, events) {
    events.forEach(inner.forEach( ev => {
        api.events.delete({
            calendarId: cal,
            eventId: ev
        });
    }));
}



// Arg processing
async function main(auth) {
    const calendar = google.calendar({ version: 'v3', auth });
    let args = argv.slice(2);
    if (args.length < 2) {
        console.log(`Usage: ./bot.js calendar spec`);
        logCals(calendar);
    } else {
        let dat = parse(args[1]);
        let cal = args[0];
        events = await get_hols(calendar, cal, dat);
        console.log(events);
        //del(calendar, cal, events);
    }
}

authorize(main);