# CPSC 312 Barbershop Scheduler

This is a group project for CPSC 312, 2021, UBC. \
We will create a customer scheduler for barber shops in Haskell language. The users will be able to choose their preferred barbers, their preferred date and time, or search for available slots.

# Contributors
Siwei, Zoe

# To run the scheduler
In Terminal, enter:\
        ghci\
        :load main\
        run


There are 2 available barbers in the application, \
you can enter "tom" or "tony" as your preferred barber, then choose your preferred date and time. 

We use real-time date data type, you can only enter a date on or after today. \
We assume the shop is closed on every Tuesday, you cannot enter a date that is on Tuesday.\
To test this, try enter the date "2021-03-09" which is a Tuesday.

If you enter a time that is occupied, we will try to find the nearest available time in the same day to schedule.\
To test this, try enter the date "2021-11-11", barber "tom", then "10:00" which is an unavailable time, you will see the nearest available time we found on that day and you can book a schedule on the available time. 

Enter "q" or "quit" to quit the program.
