import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
import time

driver = webdriver.Chrome()
driver.get("https://www.tabroom.com/index/results/")

#change display of dropdowns
element = driver.find_element(by=By.NAME, value="state")
driver.execute_script("arguments[0].style.display = 'block';", element)

element = driver.find_element(by=By.NAME, value="year")
driver.execute_script("arguments[0].style.display = 'block';", element)

#select state
state = Select(driver.find_element(by=By.NAME, value="state"))
state.select_by_value("SD")

#select season--this doesn't actually do anything because tab is broken lol, 
#it ends up displaying results from all years
year = Select(driver.find_element(by=By.NAME, value="year"))
year.select_by_value("2021")

#submit form
driver.find_element(by=By.XPATH, value="//input[@value='Search']").submit() 

#read results table
table = driver.find_element(by=By.ID, value="results")
tbody = table.find_element(By.TAG_NAME, "tbody")
rows = tbody.find_elements(By.TAG_NAME, "tr")

#store tournament links in array
links = []
for row in rows:     
    col = row.find_elements(By.TAG_NAME, "td")[4]
    link = col.find_element(By.TAG_NAME, "a").get_attribute("href")
    links.append(link)

#create list to store tables 
tables = []

#function to go through rounds of an event
def select(event, name):

    #change dropdown visibility
    element = driver.find_element(by=By.NAME, value="event_id")
    driver.execute_script("arguments[0].style.display = 'block';", element)

    #get option elements
    options = driver.find_elements(by=By.XPATH, value="//select[@name = 'event_id']/option")

    try:
        #search name text in options
        for option in options:
            if name in option.text:
                event_name = option.text
                option.click()

                #get links of rounds in sidebar
                rounds = driver.find_elements(by=By.PARTIAL_LINK_TEXT, value="Round results")

                #loop through rounds and retrieve data
                for i in range(len(rounds)):
                    if i > 0:
                        rounds[i].click()
                        driver.back()
                    table = pd.read_html(driver.page_source)
                    table = table[0]
                    table['event'] = event
                    table['year'] = driver.find_element(by=By.TAG_NAME, value="h5").text
                    table['tournament'] = driver.find_element(by=By.TAG_NAME, value="h2").text
                    table['event_name'] = event_name
                    tables.append(table)

    except:
        driver.back()

#get the data!
for i in range(len(links)): 
    driver.get(links[i])

    select("LD", "Lincoln")
    select("LD", "LD")
    select("PF", "Public")
    select("PF", "PF")

#merge dataframes
debate = pd.concat(tables)
print(debate)

debate.to_csv('debate.csv')

