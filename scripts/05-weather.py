import pandas as pd

# Step 1: Read the Excel file with multiple sheets
excel_file = pd.ExcelFile('./additional_data/raw/raw_extreme_weather.xlsx')

# Step 2: Initialize a list to store quarterly counts for each city
quarterly_counts = []

# Step 3: Process each sheet (city)
for city in excel_file.sheet_names:
    # Read the sheet without headers
    df = pd.read_excel(excel_file, sheet_name=city, header=None)
    
    # Step 4: Convert the date column (fourth column, index 3) to datetime
    df[3] = pd.to_datetime(df[3], format='%m/%d/%Y')
    
    # Step 5: Extract quarter and year from the date
    df['Quarter'] = df[3].dt.quarter
    df['Year'] = df[3].dt.year
    
    # Step 6: Count unique dates per quarter for this city
    quarterly = df.groupby(['Year', 'Quarter'])[3].nunique().reset_index()
    quarterly = quarterly.rename(columns={3: 'Count of Extreme Weather Event'})
    
    # Step 7: Generate all possible year-quarter combinations for this city up to 2024
    # Get the minimum year in the data for this city
    min_year = df['Year'].min()
    # Set the maximum year to 2024
    max_year = 2024
    
    # Create a DataFrame with all possible year-quarter combinations from min_year to 2024
    all_combinations = pd.DataFrame(
        [(year, quarter) for year in range(min_year, max_year + 1) for quarter in range(1, 5)],
        columns=['Year', 'Quarter']
    )
    
    # Step 8: Merge the actual counts with all possible combinations, filling missing counts with 0
    quarterly = all_combinations.merge(quarterly, on=['Year', 'Quarter'], how='left')
    quarterly['Count of Extreme Weather Event'] = quarterly['Count of Extreme Weather Event'].fillna(0).astype(int)
    
    # Step 9: Add city and format the Quarter column
    quarterly['City'] = city
    quarterly['Quarter'] = quarterly['Quarter'].apply(lambda x: f"Q{x}")
    
    # Append to the list of quarterly counts
    quarterly_counts.append(quarterly[['City', 'Quarter', 'Year', 'Count of Extreme Weather Event']])

# Step 10: Combine the quarterly counts into a DataFrame
quarterly_summary = pd.concat(quarterly_counts, ignore_index=True)

# Step 11: Create a new Excel file with the summary
with pd.ExcelWriter('./additional_data/raw/extreme_weather.xlsx', engine='xlsxwriter') as writer:
    # Write quarterly summary to a sheet
    quarterly_summary.to_excel(writer, sheet_name='Quarterly Summary', index=False)

print("Quarterly Summary has been created.")