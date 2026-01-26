#!/bin/bash
# Export Docker environment variables to file
cat /proc/1/environ | tr "\0" "\n" | grep -E "^(DB_|EMAIL_|TZ)" > /tmp/docker.env
