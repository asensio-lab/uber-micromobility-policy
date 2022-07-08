import pandas as pd
import datetime
import plotly.express as px

#Place both relevant datasets within the same directory

#-------------------------------------------
#Figure S1
#-------------------------------------------

#-------------------------------------------
#Figure S1A
#-------------------------------------------

df_midtownddd = pd.read_csv(r'MidtownDDD_production.csv')

def create_means(df):
    mean_list_pre = []
    mean_list_post = []
    df_pre = df[df['post']==0]
    df_post = df[df['post']==1]
    for date in df_pre['Date'].unique():
        mean = df_pre[df_pre['Date']==date]['traveltime.mile'].mean()
        mean_list_pre.append((date, mean))
        
    for date in df_post['Date'].unique():
        mean = df_post[df_post['Date']==date]['traveltime.mile'].mean()
        mean_list_post.append((date, mean))
        
    return mean_list_pre, mean_list_post

df_midtown = df_midtownddd[df_midtownddd["origin.area"]=='m']
df_cumber = df_midtownddd[df_midtownddd["origin.area"]=='c']
df_buckhead = df_midtownddd[df_midtownddd["origin.area"]=='b']
df_sandy = df_midtownddd[df_midtownddd["origin.area"]=='s']

midtown_pre, midtown_post = create_means(df_midtown)
cumber_pre, cumber_post = create_means(df_cumber)
buckhead_pre, buckhead_post = create_means(df_buckhead)
sandy_pre, sandy_post = create_means(df_sandy)
    
d_midtown_pre = {'date': [x[0] for x in midtown_pre], 'minutes_per_mile': [x[1] for x in midtown_pre], 'type': ["midtown_pre" for x in midtown_pre]}
d_midtown_post = {'date': [x[0] for x in midtown_post], 'minutes_per_mile': [x[1] for x in midtown_post], 'type': ["Midtown" for x in midtown_post]}

d_cumber_pre = {'date': [x[0] for x in cumber_pre], 'minutes_per_mile': [x[1] for x in cumber_pre], 'type': ["cumber_pre" for x in cumber_pre]}
d_cumber_post = {'date': [x[0] for x in cumber_post], 'minutes_per_mile': [x[1] for x in cumber_post], 'type': ["Cumberland" for x in cumber_post]}

d_buckhead_pre = {'date': [x[0] for x in buckhead_pre], 'minutes_per_mile': [x[1] for x in buckhead_pre], 'type': ["buckhead_pre" for x in buckhead_pre]}
d_buckhead_post = {'date': [x[0] for x in buckhead_post], 'minutes_per_mile': [x[1] for x in buckhead_post], 'type': ["Buckhead" for x in buckhead_post]}

d_sandy_pre = {'date': [x[0] for x in sandy_pre], 'minutes_per_mile': [x[1] for x in sandy_pre], 'type': ["sandy_pre" for x in sandy_pre]}
d_sandy_post = {'date': [x[0] for x in sandy_post], 'minutes_per_mile': [x[1] for x in sandy_post], 'type': ["Sandy Springs" for x in sandy_post]}
                                   
df_midtown_pre = pd.DataFrame(data=d_midtown_pre)
df_midtown_post = pd.DataFrame(data=d_midtown_post)

df_cumber_pre = pd.DataFrame(data=d_cumber_pre)
df_cumber_post = pd.DataFrame(data=d_cumber_post)

df_buckhead_pre = pd.DataFrame(data=d_buckhead_pre)
df_buckhead_post = pd.DataFrame(data=d_buckhead_post)

df_sandy_pre = pd.DataFrame(data=d_sandy_pre)
df_sandy_post = pd.DataFrame(data=d_sandy_post)

df_midtown = df_midtown_pre.append(df_midtown_post)
df_cumber = df_cumber_pre.append(df_cumber_post)
df_buckhead = df_buckhead_pre.append(df_buckhead_post)
df_sandy = df_sandy_pre.append(df_sandy_post)

df_bottom = df_midtown.append(df_cumber)
df_top = df_buckhead.append(df_sandy)
df_fig_midtown = df_bottom.append(df_top)

df_fig_midtown['date_format'] = df_fig_midtown['date'].apply(lambda x: datetime.datetime(2000 + int(x.split('/')[2]), int(x.split('/')[0]), int(x.split('/')[1])))


fig_midtown = px.scatter(
    df_fig_midtown, x="date_format", y="minutes_per_mile", color="type", 
    color_discrete_sequence=["#0099C6", "#0099C6", "#DC3912", "#DC3912", "#AB63FA", "#AB63FA", "#2CA02C", "#2CA02C"], 
    trendline='lowess', trendline_options=dict(frac=0.7), 
    symbol_sequence = ['circle-open'],
    width=1000, height=750
)

fig_midtown.update_layout({
    "plot_bgcolor": "rgba(0, 0, 0, 0)",
    "paper_bgcolor": "rgba(0, 0, 0, 0)",
})

fig_midtown.update_xaxes(showline=True, linewidth=2, linecolor='black', mirror=True)
fig_midtown.update_yaxes(showline=True, linewidth=2, linecolor='black', mirror=True)
fig_midtown.for_each_trace(lambda t: t.update(name = '<b>' + t.name +'</b>'))    

for trace in fig_midtown['data']:
    if 'pre' in trace['name']:
        trace['showlegend'] = False
        
fig_midtown.update_layout(legend={'title_text':''})
fig_midtown.update_layout(
    legend=dict(
        yanchor="top",
        y=0.99,
        xanchor="left",
        x=0.01
    )
)

fig_midtown.update_xaxes(showgrid=False)
fig_midtown.update_yaxes(showgrid=False)

fig_midtown.add_annotation(
    text="<b>A<b>", 
    xref="paper", 
    yref="paper", 
    x=0.99, y=0.99, 
    font=dict(
        family="Arial",
        size=30,
        color="#000000"
    ), showarrow=False
)

fig_midtown.update_layout(
yaxis = dict(
tickfont = dict(size=20)))

fig_midtown.update_layout(
xaxis = dict(
tickfont = dict(size=20)))

fig_midtown.update_layout(
    yaxis = dict(
        tickmode = 'array',
        tickvals = [2, 2.5, 3, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0],
        ticktext = ['2.0', '2.5', '3.0', '3.5', '4.0', '4.5', '5.0', '5.5', '6.0']
    )
)

fig_midtown.update_layout(
    legend=dict(
        orientation="h",
        yanchor="bottom",
        y=1.02,
        xanchor="right",
        x=1
    )
)

fig_midtown.update_layout(
    title="<b>Midtown Experiment</b>",
    xaxis_title="<b>Date</b>",
    yaxis_title="<b>Travel Time per Mile (min/mile)</b>",
    font=dict(
        family="Arial",
        size=18,
        color="black"
    )
)

fig_midtown.add_vline(x=datetime.datetime(2019, 8, 8, 12, 0), line_width=3, line_dash="solid", line_color="gray")
fig_midtown.update_traces(marker_size=9)
fig_midtown.show()


#-------------------------------------------
#Figure S1B
#-------------------------------------------

df_marta = pd.read_csv(r'MARTA_production_final.csv', encoding='cp1252')

avg_list_control_pre = []
avg_list_control_post = []
avg_list_treatment_pre = []
avg_list_treatment_post = []

df_control_pre = df_marta[(df_marta['treatment']==0) & (df_marta['post']==0)]
df_control_post = df_marta[(df_marta['treatment']==0) & (df_marta['post']==1)]
df_treatment_pre = df_marta[(df_marta['treatment']==1) & (df_marta['post']==0)]
df_treatment_post = df_marta[(df_marta['treatment']==1) & (df_marta['post']==1)]

for date in df_marta[df_marta['post']==0]['Date'].unique():
    mean = df_control_pre[df_control_pre['Date']==date]['Travel.Time.per.Mile..Minutes.Mile.'].mean()
    avg_list_control_pre.append((date, mean))
    
for date in df_marta[df_marta['post']==1]['Date'].unique():
    mean = df_control_post[df_control_post['Date']==date]['Travel.Time.per.Mile..Minutes.Mile.'].mean()
    avg_list_control_post.append((date, mean))
    
for date in df_marta[df_marta['post']==0]['Date'].unique():
    mean = df_treatment_pre[df_treatment_pre['Date']==date]['Travel.Time.per.Mile..Minutes.Mile.'].mean()
    avg_list_treatment_pre.append((date, mean))
    
for date in df_marta[df_marta['post']==1]['Date'].unique():
    mean = df_treatment_post[df_treatment_post['Date']==date]['Travel.Time.per.Mile..Minutes.Mile.'].mean()
    avg_list_treatment_post.append((date, mean))
    
d_control_pre = {'date': [x[0] for x in avg_list_control_pre], 'minutes_per_mile': [x[1] for x in avg_list_control_pre], 'type': ["control_pre" for x in avg_list_control_pre]}
d_control_post = {'date': [x[0] for x in avg_list_control_post], 'minutes_per_mile': [x[1] for x in avg_list_control_post], 'type': ["No-Policy Zone" for x in avg_list_control_post]}
d_treatment_pre = {'date': [x[0] for x in avg_list_treatment_pre], 'minutes_per_mile': [x[1] for x in avg_list_treatment_pre], 'type': ["treatment_pre" for x in avg_list_treatment_pre]}
d_treatment_post = {'date': [x[0] for x in avg_list_treatment_post], 'minutes_per_mile': [x[1] for x in avg_list_treatment_post], 'type': ["Policy Zone" for x in avg_list_treatment_post]}
                                   
df_control_pre = pd.DataFrame(data=d_control_pre)
df_control_post = pd.DataFrame(data=d_control_post)
df_treatment_pre = pd.DataFrame(data=d_treatment_pre)
df_treatment_post = pd.DataFrame(data=d_treatment_post)

df_fig_marta = df_control_pre.append(df_control_post)
df_fig_marta = df_fig_marta.append(df_treatment_pre)
df_fig_marta = df_fig_marta.append(df_treatment_post)

df_fig_marta['date_format'] = df_fig_marta['date'].apply(lambda x: datetime.datetime(2000 + int(x.split('/')[2]), int(x.split('/')[0]), int(x.split('/')[1])))

df_fig_marta = df_fig_marta[(df_fig_marta['date_format'] >= datetime.datetime(2019, 7, 12)) & (df_fig_marta['date_format'] <= datetime.datetime(2019, 9, 6))]

fig_marta = px.scatter(
    df_fig_marta, x="date_format", y="minutes_per_mile", color="type",
    color_discrete_sequence=["#DC3912", "#DC3912", "#0099C6", "#0099C6"], 
    trendline='lowess', trendline_options=dict(frac=0.75), 
    symbol_sequence = ['circle-open'],
    width=1000, height=750
)

fig_marta.update_layout({
    "plot_bgcolor": "rgba(0, 0, 0, 0)",
    "paper_bgcolor": "rgba(0, 0, 0, 0)",
})

for trace in fig_marta['data']:
    if(trace['name'] == 'control_pre' or trace['name'] == 'treatment_pre'):
        trace['showlegend'] = False

fig_marta.update_layout(legend={'title_text':''})
fig_marta.update_layout(
    legend=dict(
        yanchor="top",
        y=0.99,
        xanchor="left",
        x=0.01
    )
)

fig_marta.update_xaxes(showline=True, linewidth=2, linecolor='black', mirror=True)
fig_marta.update_yaxes(showline=True, linewidth=2, linecolor='black', mirror=True)
fig_marta.for_each_trace(lambda t: t.update(name = '<b>' + t.name +'</b>'))    

fig_marta.update_layout(
    title="<b>MARTA</b>",
    xaxis_title="<b>Date</b>",
    yaxis_title="<b>Travel Time per Mile (min/mile)</b>",
)

fig_marta.update_xaxes(showgrid=False)
fig_marta.update_yaxes(showgrid=False)

fig_marta.add_annotation(
    text="<b>B<b>", 
    xref="paper", yref="paper", 
    x=0.99, y=0.99, 
    font=dict(
        family="Arial",
        size=30,
        color="#000000"
    ), showarrow=False
)

fig_marta.update_layout(
yaxis = dict(
tickfont = dict(size=20)))

fig_marta.update_layout(
xaxis = dict(
tickfont = dict(size=20)))

fig_marta.update_layout(
    yaxis = dict(
        tickmode = 'array',
        tickvals = [3.5, 4.0, 4.5, 5.0, 5.5],
        ticktext = ['3.5', '4.0', '4.5', '5.0', '5.5']
    )
)

fig_marta.update_layout(
    legend=dict(
        orientation="h",
        yanchor="bottom",
        y=1.02,
        xanchor="right",
        x=1
    )
)

fig_marta.update_layout(
    title="<b>MARTA Experiment</b>",
    xaxis_title="<b>Date</b>",
    yaxis_title="<b>Travel Time per Mile (min/mile)</b>",
    font=dict(
        family="Arial",
        size=17,
        color="black"
    )
)

fig_marta.add_vline(x=datetime.datetime(2019, 8, 8, 12, 0), line_width=3, line_dash="solid", line_color="gray")
fig_marta.update_traces(marker_size=9)
fig_marta.show()
