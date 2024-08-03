"""
send_notification.py

A script to send notifications to an IRC channel using settings from a .env file.

Author: Jason Walsh <j@wal.sh>
"""

import os
import socket
from dotenv import load_dotenv

def load_env_variables() -> None:
    """Load environment variables from .env file."""
    load_dotenv()

def get_env_variable(var_name: str, default: str = '') -> str:
    """Retrieve environment variable or return default value.
    
    Args:
        var_name (str): The name of the environment variable.
        default (str): The default value to return if the variable is not found.
    
        Returns:
            str: The value of the environment variable.
    """
    return os.getenv(var_name, default)

def send_irc_message(server: str, port: int, channel: str, nick: str, user: str, password: str, message: str) -> None:
    """Send a message to an IRC channel.
    
    Args:
        server (str): IRC server address.
        port (int): IRC server port.
        channel (str): IRC channel to join.
        nick (str): IRC nickname.
        user (str): IRC username.
        password (str): IRC password.
        message (str): Message to send.
    """
    irc = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    irc.connect((server, port))
    
    # Authenticate
    irc.send(f"PASS {password}\n".encode('utf-8'))
    irc.send(f"NICK {nick}\n".encode('utf-8'))
    irc.send(f"USER {user} 0 * :{user}\n".encode('utf-8'))
    
    # Join channel
    irc.send(f"JOIN {channel}\n".encode('utf-8'))
    
    # Send message
    irc.send(f"PRIVMSG {channel} :{message}\n".encode('utf-8'))
    
    # Close connection
    irc.send(b"QUIT\n")
    irc.close()

if __name__ == "__main__":
    load_env_variables()

    SERVER = get_env_variable('IRC_NOTIFICATION_SERVER')
    PORT = int(get_env_variable('IRC_NOTIFICATION_PORT'))
    CHANNEL = get_env_variable('IRC_NOTIFICATION_CHANNEL')
    NICK = get_env_variable('IRC_NOTIFICATION_NICK')
    USER = get_env_variable('IRC_NOTIFICATION_USER')
    PASSWORD = get_env_variable('IRC_NOTIFICATION_PASSWORD')

    message = "This is a test notification."
    
    print("Environment Settings:")
    print(f"Server: {SERVER}")
    print(f"Port: {PORT}")
    print(f"Channel: {CHANNEL}")
    print(f"Nick: {NICK}")
    print(f"User: {USER}")
    
    confirmation = input(f"\nDo you want to send the following message to {CHANNEL}? \n'{message}'\n(y/n): ")
    if confirmation.lower() == 'y':
        send_irc_message(SERVER, PORT, CHANNEL, NICK, USER, PASSWORD, message)
        print(f"Notification sent to {CHANNEL}: {message}")
    else:
        print("Notification cancelled.")
