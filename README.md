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
you can enter "tom" or "tony" as your preferred barber, then choose your preferred schedule time. \
If you enter a time that is occupied, we will try to find the nearest available time to schedule.

# Problems need to be fixed
1:\
Everytime I add a new schedule, the names of old schedules would changed\
eg.\
When I add new schedule to 16:00 for a name "Lin"\
10:00-----"\"Cat\""\
12:00-----"\"Amy\" "\
14:00-----"\"David\""\
16:00-----"Lin"

2:\
new features can be added:\
print all schedules for all barbers in the main menu?\
cancel a schedule\
if you enter 14:00 for Tom and it is unavailable to schedule, check if other barbers' 14:00 are available?

3:\
exception should not appeared in the interface
