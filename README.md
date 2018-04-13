# whatsapp-analyser

Use this script if you want to analyse your WhatsApp conversations (also group conversations). To run this script you have to provide the "_chat.txt" file from WhatsApp.
You can get this file by exporting your WhatsApp conversation from within your smartphone app (it's not possible to access this file from the Web App.

# Limitations

This script is currently running only with exports from iOS in german. Slight modifications needed for other smartphon operating systems and other languages (different time and date formats).

# What does this script do?

The WhatsApp export file is pretty messy. This script cleans the file, profiding structure for your further research within R.
It also generates different graphics and plots: 
 * Messages per weekday
 * Messages per hour
 * Messages per user
 * Emoji usage per user
 * Wordcloud of all messages
 * Most typical words by user
 * User activity per day
 * User activity per hour

