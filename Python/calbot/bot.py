from __future__ import print_function
import datetime
from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
import sys
import parser

def sig(func):
    """Print the decorated function's running signature.
    """
    name = func.__name__
    def wrap(*args):
        argstr = ", ".join(str(arg) for arg in args)
        result = func(*args)
        print("%s(%s):\t%s" % (name, argstr, result))
        return result
    return wrap

if len(sys.argv) < 3:
    if len(sys.argv) == 1:
        print("Descriptor file needed.")
        sys.exit(1)
    print("Final arg should be calendar.")

data = parser.parse(sys.argv[1])

#### OAUTH ####
# If modifying these scopes, delete the file token.json.
SCOPES = ["https://www.googleapis.com/auth/calendar.readonly",
        "https://www.googleapis.com/auth/calendar.events"]
store = file.Storage('token.json')
creds = store.get()
if not creds or creds.invalid:
    flow = client.flow_from_clientsecrets('credentials.json', SCOPES)
    creds = tools.run_flow(flow, store)
service = build('calendar', 'v3', http=creds.authorize(Http()))

#### USAGE ####
def getCal(name):
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        if entry['summary'] == name:
            return entry['id']

def recurring(cal, data):
    """Get recurring instances."""
    events = set()
    for start, end in data:
        response = service.events().list(
                calendarId=cal,
                timeMin=start, timeMax=end).execute()
        for event in response['items']:
            try:
                event['recurrence']
                events.add(event['id'])
            except KeyError:
                break
    return events

def expand(cal, recurring, data):
    """Expand recurrences to ranged instances"""
    events = set()
    for recur in recurring:
        for start, end in data:
            response = service.events().instances(
                    timeMin=start, timeMax=end,
                    calendarId=cal, eventId=recur).execute()
            for event in response['items']:
                events.add(event['id'])
    return events

def getEvents(cal, data):
    return expand(cal, recurring(cal, data), data)

def delEvents(calID, events):
    num = 0
    for event in events:
        service.events().delete(calendarId=calID, eventId=event).execute()
        num += 1
    return num

if len(sys.argv) < 3: # enumerate calendar id's
    print("Calendars:")
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        print("\t%s" % entry['summary'])
else: # delete
    calID = getCal(sys.argv[2])
    expanded = getEvents(calID, data)
    deletions = delEvents(calID, expanded)
    print("%d Events deleted from '%s'" % (deletions, sys.argv[2]))
