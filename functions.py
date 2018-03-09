def bivar_plt(data,attribute,title_size=20,fig_width=12,fig_height=4,bar_width=0.3):
    fig, axs = plt.subplots(1,2,figsize=(fig_width,fig_height),squeeze=0)
    
    pd.crosstab(data[attribute],data.return_status_repurchase,normalize='index').plot(kind='bar',width=bar_width,ax=axs[0][0],
                                                                                      color=['red','blue'])
    axs[0, 0].set_title( attribute+ 'VS Returns_',size=title_size,fontname="Arial",fontweight="bold")
    plt.setp(axs[0, 0].get_xticklabels(),rotation=45,size=12)
    axs[0, 0].set_xlabel(attribute,size=10,fontname="Calibri")
    axs[0,0].set_ylabel('Proportion',size=10,fontname="Calibri")
    #plt.xlabel(fontsize=10)
    

    axs[0,0].legend(bbox_to_anchor=(1.1, 1.05),loc='right')
    data[attribute].value_counts().plot(kind='bar',ax=axs[0][1],width=0.3)
    plt.title('Frequency of Categories',size=20,fontname="Arial",fontweight="bold")
    plt.setp(axs[0, 1].get_xticklabels(),rotation=45,size=12)
    axs[0,1].set_xlabel(attribute,size=15,fontname="Calibri")
    axs[0,1].set_ylabel('Frequency',size=15,fontname="Calibri")
bivar_plt(data=new_data,attribute='PARENT_MDSE_DIVN_DESC',fig_height=4,fig_width=17,title_size=13,bar_width=0.5)

def bivar_plt_new(data,attribute,title_size=20,fig_width=12,fig_height=4,bar_width=0.3):
    fig, axs = plt.subplots(1,2,figsize=(fig_width,fig_height),squeeze=0)
    
#     pd.crosstab(data[attribute],data.return_status_repurchase,normalize='index').plot(kind='bar',width=bar_width,ax=axs[0][0],color=['#FF6D31','#29A2C6'])
#     axs[0, 0].set_title( attribute+ ' vs Target',size=title_size,fontname="Arial",fontweight="bold")
#     plt.setp(axs[0, 0].get_xticklabels(),rotation=45,size=12)
#     axs[0, 0].set_xlabel(attribute,size=15,fontname="Calibri")
#     axs[0,0].set_ylabel('Proportion',size=15,fontname="Calibri")
    
    sorted_names = data[attribute].value_counts().index
    temp_df = pd.crosstab(data[attribute],data.return_status_repurchase,normalize='index')
    temp_df.reset_index(inplace=True)
    temp_df.columns = ['Levels','yes_prop','No_prop']
    temp_df2 = pd.DataFrame(sorted_names)
    temp_df2.columns = ['Levels']
    df_Final = pd.merge(temp_df2,temp_df,left_on='Levels',right_on='Levels')
    df_Final.plot(kind='bar',stacked=True,ax=axs[0,0])
    x=[0,1,2,3,4,5,6]
    plt.xticks(x, df_Final.Levels, rotation='vertical')
    plt.show()

    data[attribute].value_counts().plot(kind='bar',ax=axs[0,1],width=0.3,color='#6bb69c')
    plt.title('Frequency of Categories',size=20,fontname="Arial",fontweight="bold")
    plt.setp(axs[0,1].get_xticklabels(),rotation=45,size=12)
    axs[0,1].set_xlabel(attribute,size=15,fontname="Calibri")
    axs[0,1].set_ylabel('Frequency',size=15,fontname="Calibri")
    
    
    
bivar_plt(data=new_data,attribute='GMM_DESC',bar_width=0.6)


######--------Proporiton Stacked Charts----------#################

plt.figure(figsize=(5,5))
data = reshaped_data
attribute = 'GMM_DESC'
sorted_names = data[attribute].value_counts().index
temp_df = pd.crosstab(data[attribute],data.return_status_repurchase,normalize='index')
temp_df.reset_index(inplace=True)
temp_df.columns = ['Levels','yes_prop','No_prop']
temp_df2 = pd.DataFrame(sorted_names)
temp_df2.columns = ['Levels']
df_Final = pd.merge(temp_df2,temp_df,left_on='Levels',right_on='Levels')
df_Final.plot(kind='bar',stacked=True,legend=None)
x=[0,1,2,3,4,5,6]
plt.xticks(x, df_Final.Levels, rotation='vertical')
plt.show()

def roc_cutoffs(model,X,y):
    df=pd.DataFrame()
    i=0
    df['cutoff']=np.arange(0.1,1.0,0.1)
    
    for cutoff in np.arange(0.1,1.0,0.1):
        x = pd.DataFrame(model.predict_proba(X))[1].map(lambda x: 1 if x>cutoff else 0)
        cf=confusion_matrix(y,x)
        TP=cf[1,1]
        FP=cf[0,1]
        TN=cf[0,0]
        FN=cf[1,0]
        TPR = TP/(TP+FN)
        FPR = FP/(TN+FP)
        df.loc[i,'Accuracy_Score']=accuracy_score(y,x)
        df.loc[i,'TPR']=TPR
        df.loc[i,'FPR']=FPR
        i=i+1
    return df
	
#pd.DataFrame(classification_report(y_train,model_gbm.predict(X_train)))
def classifaction_report_csv(report):
    report_data = []
    lines = report.split('\n')
    for line in lines[2:-3]:
        row = {}
        row_data = line.split('      ')
        row['class'] = row_data[0]
        row['precision'] = float(row_data[1])
        row['recall'] = float(row_data[2])
        row['f1_score'] = float(row_data[3])
        row['support'] = float(row_data[4])
        report_data.append(row)
    dataframe = pd.DataFrame.from_dict(report_data)
    return dataframe

def classifaction_report_csv(report):
    report_data = []
    lines = report.split('\n')
    for line in lines[2:-3]:
        row = {}
        row_data = line.split('      ')
        row['class'] = row_data[0]
        row['precision'] = float(row_data[1])
        row['recall'] = float(row_data[2])
        row['f1_score'] = float(row_data[3])
        row['support'] = float(row_data[4])
        report_data.append(row)
    dataframe = pd.DataFrame.from_dict(report_data)
    return dataframe
