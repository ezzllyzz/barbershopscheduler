# CPSC 312 Barbershop Scheduler

This is a group project for CPSC 312, 2021, UBC. 

We will create a customer scheduler for barber shops in Haskell language. The users will be able to choose their barbers, their preferred time, or search for available slots.

# Contributors
Siwei, Zoe

# To run the scheduler
In Terminal, enter   
        ghci
        :load main
        run


There are 2 barbers in the application, you can enter "tom" or "tony" then choose your preferred schedule. 

If you enter a time that is occupied, we will try to find the nearest available time to schedule.

# Problems need to be fixed

Everytime I add a new schedule, the names of old schedules would changed

eg.
When I add new schedule to 16:00 for a name "Lin"

10:00-----"\"Cat\""
12:00-----"\"Amy\" "
14:00-----"\"David\""
16:00-----"Lin"
