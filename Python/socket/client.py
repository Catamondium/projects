import socket

clidata = socket.gethostname(), 5007

with socket.socket() as client:
    client.connect(clidata)
    msg = client.recv(24)
print("Recieved:\t%s" % msg.decode(), end='')
