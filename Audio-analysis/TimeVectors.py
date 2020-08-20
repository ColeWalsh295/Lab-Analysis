import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def calc_consistency_equity(file, interval):
    """
    Produces plots of speaking equity and consistency over time.

    Keyword arguments:
    file -- exported BORIS data (note that headers used here should be altered to agree with file or vice-versa)
    interval -- time interval (in seconds) to examine speaking shares
    """
    
    plt.style.use('ggplot')
    df = pd.read_csv(file)

    Subjects = list(set(df['Subject'])) # get unique individuals in data
    TimeEnd = int(df['Time Clip'].tail(1))
    Interval = int(interval)
    TimeStart = np.arange(0, int(round((TimeEnd/Interval) + 1) * Interval), Interval) # beginning of all separate time intervals
    Vectors = []
    for TimeIndex, Time in enumerate(TimeStart):
        SpeakingTime = np.zeros(len(Subjects)) # initialize speaking time to zero for everybody in an interval
        for Index, Student in enumerate(Subjects):
            SplitDF = df[(df['Time Clip'] >= Time) & (df['Time Clip'] < Time + Interval) & (df['Subject'] == Student)] # dataframe of times within interval
            SplitDF = SplitDF.reset_index(drop = True)
            if(SplitDF.empty):
                SpeakingTime[Index] = 0
                continue

            if(SplitDF['Start/Stop'][0] == 'STOP'): # if the first entry in interval is 'STOP', we add time from beginning of interval
                SpeakingTime[Index] = SpeakingTime[Index] + (SplitDF['Time Clip'][0] - Time)
                SplitDF = SplitDF.iloc[1:] # then first entry will be a 'START'
                SplitDF = SplitDF.reset_index(drop = True)
            if(SplitDF['Start/Stop'].tail(1).item() == 'START'): # if last entry in interval is 'START', we add time to end of interval
                SpeakingTime[Index] = SpeakingTime[Index] + ((Time + Interval) - SplitDF['Time Clip'].tail(1))
                SplitDF = SplitDF.iloc[:-1] # then last entry will be a 'STOP'
                SplitDF = SplitDF.reset_index(drop = True)
            Num = int(len(SplitDF.index)/2)
            assert(Num % 1 == 0) # if equal number of 'START' and 'STOP' remaining in interval
            for j in range(Num): # add up speaking time between 'START' and 'STOP'
                SpeakingTime[Index] = SpeakingTime[Index] + (SplitDF.loc[2 * j + 1, 'Time Clip'] - SplitDF.loc[2 * j, 'Time Clip'])
        Vectors.append(SpeakingTime)

    Norms = np.empty(len(Vectors))
    ConDot = np.empty(len(Vectors) - 1)
    Norms[0] = np.linalg.norm(Vectors[0], ord = 2)
    for Index, Vector in enumerate(Vectors[1:]):
        Norms[Index + 1] = np.linalg.norm(Vector, ord = 2) # normalize the speaking vector in each interval
        ConDot[Index] = np.dot((1/Norms[Index]) * Vectors[Index], (1/Norms[Index + 1]) * Vectors[Index + 1]) # consistency of speaking with previous interval

    EqVec = np.full(len(Vectors[0]), np.sqrt(1/len(Vectors[0]))) # what does equal speaking look like
    EqDot = [np.dot(EqVec, (1/Norms[i]) * Vectors[i]) for i in range(len(Vectors))] # inner product gives how close speaking shares are to equal

    # overall consistency and speaking inner products
    ConMean = np.mean(ConDot)
    EqMean = np.mean(EqDot)

    Consistency = plt.plot(TimeStart[1:], ConDot, color = 'blue', linestyle = 'solid', marker = 'o', label = 'Consistency')
    Equality = plt.plot(TimeStart, EqDot, color = 'green', linestyle = 'solid', marker = 'o', label = 'Equality')
    plt.axhline(y = ConMean, xmin = TimeStart[0], xmax = TimeEnd, linestyle = '--', color = 'blue')
    plt.axhline(y = EqMean, xmin = TimeStart[0], xmax = TimeEnd, linestyle = '--', color = 'green')
    plt.xlabel('Time')
    plt.ylabel('Inner Product')
    plt.legend()
    plt.show()
