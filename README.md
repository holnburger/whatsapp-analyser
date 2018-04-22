# whatsapp-analyser

If you want to analyse your WhatsApp conversations (also group conversations).
Analyse the "_chat.txt" by exporting it from within the smartphone App.

# Limitations

This script is currently only running with exports in german. Slight modifications needed if you want to analyse other languages (different time and date formats).

# Features

The WhatsApp export file is pretty messy. This script cleans the file, profiding structure for your further research within R.
It also generates following plots: 
 * Messages per weekday
 * Messages per hour
 * Messages per user
 * Emoji usage per user
 * Wordcloud of all messages
 * Most typical words by user
 * User activity per day
 * User activity per hour
 * iGraph network of user mentions
