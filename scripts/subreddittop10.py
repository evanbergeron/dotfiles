import praw

r = praw.Reddit(user_agent='Test script by /u/klekticist')

subs = ['futurebeats', 
        'vim', 
        'news']

def mypprint(submissions):
    for submission in submissions:
        print str(submission)

def getTop(limit, subname):
    return r.get_subreddit(subname).get_hot(limit=limit)

def printTitle(sub):
    barLength = (78 - len(sub)) / 2
    print '=' * barLength + ' %s ' % sub + '=' * barLength

def main():
    for sub in subs:
        printTitle(sub)
        mypprint(getTop(10, sub))

main()
