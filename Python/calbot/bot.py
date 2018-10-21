#!./venv/bin/python
from __future__ import print_function
from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
import sys
import parser

SCOPES = ["https://www.googleapis.com/auth/calendar.readonly",
        "https://www.googleapis.com/auth/calendar.events"]

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

def connect():
    """Connect calendar API."""
    store = file.Storage('token.json')
    creds = store.get()
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets('credentials.json', SCOPES)
        creds = tools.run_flow(flow, store) # fails
    return build('calendar', 'v3', http=creds.authorize(Http()))

def getCal(service, name):
    """Get calendar ID by name."""
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        if entry['summary'] == name:
            return entry['id']

def printCals(service):
    """Enumerate available calendars."""
    print("Calendars:")
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        print("\t%s" % entry['summary'])

def is_recurring(event):
    """True if recurring event, else False or error.
    """
    try:
        event['recurrence']
        return True
    except KeyError:
        return False

def expand(service, cal, event, holiday):
    """Expand recurring events to instances in data.
    """
    events = set()
    start, end = holiday
    response = service.events().instances(
            timeMin=start, timeMax=end,
            calendarId=cal, eventId=event).execute()
    for r in response['items']:
        events.add(r['id'])
    return events

def getEvents(service, cal, data):
    """Get recurring instances."""
    events = set()
    for start, end in data:
        response = service.events().list(
                calendarId=cal,
                timeMin=start, timeMax=end).execute()
        for event in response['items']:
            if is_recurring(event):
                events |= expand(service, cal, event['id'], (start, end))
    return events

def delEvents(service, cal, events):
    """Delete events and return number of deletions
    """
    for event in events:
        service.events().delete(calendarId=cal, eventId=event).execute()
    return len(events)

if __name__ == "__main__":
    service = connect()
    if len(sys.argv) < 3:
        if len(sys.argv) == 1:
            print("Descriptor file needed.")
            sys.exit(1)
        print("Final arg should be calendar.")
        printCals(service)
    else: # Run deletions
        data = parser.parse(sys.argv[1])
        calID = getCal(service, sys.argv[2])
        expanded = getEvents(service, calID, data)
        delEvents(service, calID, expanded)
        print("%d Events deleted from '%s'" % (len(expanded), sys.argv[2]))
