from __future__ import print_function
import datetime
from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
import parser

data = parser.parse("test")

#### OAUTH ####
# If modifying these scopes, delete the file token.json.
SCOPES = 'https://www.googleapis.com/auth/calendar.events'

store = file.Storage('token.json')
creds = store.get()
if not creds or creds.invalid:
    flow = client.flow_from_clientsecrets('credentials.json', SCOPES)
    creds = tools.run_flow(flow, store)
    service = build('calendar', 'v3', http=creds.authorize(Http()))

#### USAGE #### Extracted from Google quickstart example
for start, end in data:
    events_result = service.events().list(calendarId='primary', timeMin=start, timeMax=end
                                        ,orderBy='startTime').execute()
    events = events_result.get('items', [])

    if not events:
        print('No upcoming events found.')
    for event in events:
        sEvent = event['start'].get('dateTime', event['start'].get('date'))
        print(sEvent, event['summary'])
