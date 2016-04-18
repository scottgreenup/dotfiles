# Clear the keychain everytime we login.
keychain --clear

# Add our default key to the agent.
ssh-add ~/.ssh/id_rsa 2> /dev/null
