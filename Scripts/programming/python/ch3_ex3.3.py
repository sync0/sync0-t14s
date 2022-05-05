score = input('what is your grade?\n')
if isinstance(score, str) :
    print 'type only numbers'
    quit()
elif score < 0.0 or score > 1 :
    print 'wrong range! type a number between 0 and 1'
    quit()
else :
    if score >= 0.9 :
        print 'your score is A'
        quit()
    elif score < 0.9 and score >= 0.8 :
        print 'your score is B'
        quit()
    elif score < 0.8 and score >= 0.7 :
        print 'your score is C'
        quit()
    elif score < 0.7 and score >= 0.6 :
        print 'your score is D'
        quit()
    elif score < 0.6 :
        print 'your score is F. You have failed the course'
        quit()