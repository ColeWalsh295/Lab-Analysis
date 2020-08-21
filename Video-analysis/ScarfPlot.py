import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight')
import matplotlib
CMap = 'tab10'
Gradient = matplotlib.cm.get_cmap(CMap)
import seaborn as sns

no_interaction_list = ['TotalTime', 'StartClass', 'ClassStart'] # add codes here that are not part of the interaction behaviours, keep exit behaviours

def one_subject_scarf(file, nvid, method = 'TA', subject = 'TA', offset = 0, one_scarf = False):
    """Create scarf plot with one subject.

    Keyword arguments:
    file -- BORIS file with observations
    nvid -- number of videos used in observation
    method -- which coding method was used to create the observations
    subject -- which subject to focus on for the plot
    offset -- time offset
    one_scarf -- whether to create one scarf for all behaviours or separate scarves for each behavior
    """

    df = pd.read_csv(file, skiprows = (nvid + 14), usecols = ['Time', 'Subject', 'Behavior', 'Status']) # BORIS files have unnecssary beginning rows
    df['Time'] = df['Time'] - offset

    df = df.loc[~df['Behavior'].isin(no_interaction_list), :].reset_index(drop = True)

    i = 0

    if(method == 'scan'):
        try:
            df = df.loc[df['Subject'] == subject, :].reset_index(drop = True)
            df['behaviour_codes'] = df['Behavior'].astype('category').cat.codes
        except Exception as e:
            raise Exception("Subject isn't in the file.")

        ColorLegend = []
        while i < (len(df.index) - 1):
            x_start = df.loc[i, 'Time']
            x_end = df.loc[i + 1, 'Time']
            if(one_scarf):
                y = 0
            else:
                y = df.loc[i, 'behaviour_codes']

            behaviour = df.loc[i, 'Behavior']
            if behaviour not in ColorLegend:
                plt.plot((x_start, x_end), (y, y), linewidth = 25, color = Gradient(df.loc[i, 'behaviour_codes']/10), label = behaviour)
                ColorLegend.append(behaviour)
            else:
                plt.plot((x_start, x_end), (y, y), linewidth = 25, color = Gradient(df.loc[i, 'behaviour_codes']/10))
            i += 1

    elif(method == 'TA'):
        df['behaviour_codes'] = df['Behavior'].astype('category').cat.codes

        while i < len(df.index):
            x_start = df.loc[i, 'Time']
            x_end = df.loc[i + 1, 'Time']
            if(one_scarf):
                y = 0
            else:
                y = df.loc[i, 'behaviour_codes']

            plt.plot((x_start, x_end), (y, y), linewidth = 25, color = Gradient(y/10))
            i += 2 # TA method use START/STOP state events

    plt.gca().invert_yaxis()
    plt.xticks((0, 900, 1800, 2700, 3600, 4500, 5400, 6300), (0, 15, 30, 45, 60, 75, 90, 105))
    plt.xlabel('Time (min)')
    if(one_scarf):
        plt.yticks([0], [subject])
        plt.legend(bbox_to_anchor = (1, 0.75))
    else:
        plt.yticks(range(max(df['behaviour_codes']) + 1), sorted(df['Behavior'].unique()))
        plt.ylabel('Table')
    plt.show()

def multiple_subject_scarves(file, nvid, method = 'scan', offset = 0):
    """Create scarf plot using multiple subjects.

    Keyword arguments:
    file -- BORIS file with observations
    nvid -- number of videos used in observation
    method -- which coding method was used to create the observations
    offset -- time offset
    """

    df = pd.read_csv(file, skiprows = (nvid + 14), usecols = ['Time', 'Subject', 'Behavior', 'Status'])
    df['Time'] = df['Time'] - offset

    df = df.loc[~df['Behavior'].isin(no_interaction_list), :].reset_index(drop = True)

    ColorLegend = []
    df['behaviour_codes'] = df['Behavior'].astype('category').cat.codes
    df['subject_codes'] = df['Subject'].astype('category').cat.codes
    subjects = list(set(df['Subject']))
    for subject in subjects:
        df_subject = df.loc[df['Subject'] == subject, :].reset_index(drop = True)

        i = 0
        while i < (len(df_subject.index) - 1):
            x_start = df_subject.loc[i, 'Time']
            x_end = df_subject.loc[i + 1, 'Time']
            y = df_subject.loc[i, 'subject_codes']

            behaviour = df_subject.loc[i, 'Behavior']
            if behaviour not in ColorLegend:
                plt.plot((x_start, x_end), (y, y), linewidth = 10, color = Gradient(df_subject.loc[i, 'behaviour_codes']/10), label = behaviour)
                ColorLegend.append(behaviour)
            else:
                plt.plot((x_start, x_end), (y, y), linewidth = 10, color = Gradient(df_subject.loc[i, 'behaviour_codes']/10))
            i += 1

    plt.gca().invert_yaxis()
    plt.xticks((0, 900, 1800, 2700, 3600, 4500, 5400, 6300), (0, 15, 30, 45, 60, 75, 90, 105))
    plt.xlabel('Time (min)')
    plt.legend(bbox_to_anchor = (1, 0.75))
    plt.yticks(range(max(df['subject_codes']) + 1), sorted(df['Subject'].unique()))
    plt.ylabel('Subject')
    plt.show()
