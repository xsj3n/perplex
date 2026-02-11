import socket

host = "127.0.0.1"
port = 4321

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect((host, port))
while True:
    query = input("perplex-cli>").encode() + b"\r\n\r\n"
    sock.sendall(query)
    data = sock.recv(1028)
    print(f"rcv:\n{data.decode().strip()}\n\n")

sock.close()
