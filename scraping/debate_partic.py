import pandas as pd
import random
import string
from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.by import By

driver = webdriver.Chrome()
driver.get("https://www.tabroom.com/index/results/")

# change display of dropdowns
element = driver.find_element(By.NAME, "state")
driver.execute_script("arguments[0].style.display = 'block';", element)

element = driver.find_element(By.NAME, "year")
driver.execute_script("arguments[0].style.display = 'block';", element)

# select state
state = Select(driver.find_element(By.NAME, "state"))
state.select_by_value("SD")

# select season--this doesn't actually do anything because tab is broken lol, 
# it ends up displaying results from all years
year = Select(driver.find_element(By.NAME, "year"))
year.select_by_value("2021")

# submit form
driver.find_element(By.XPATH, "//input[@value='Search']").submit() 

# read results table
table = driver.find_element(By.ID, "results")
tbody = table.find_element(By.TAG_NAME, "tbody")
rows = tbody.find_elements(By.TAG_NAME, "tr")

# store tournament links in array
links = []
for row in rows:     
    col = row.find_elements(By.TAG_NAME, "td")[4]
    link = col.find_element(By.TAG_NAME, "a").get_attribute("href")
    links.append(link)

# create dict to store tables 
debate_partic = {}

# this is to create random unique keys in the dictionary... 
# the better way to do this is just get the table id LOL 
# but i already ran with this
def get_random_string(length):
    letters = string.ascii_lowercase
    return ''.join(random.choice(letters) for i in range(length))

# function to go through rounds of an event
def select(event, name):

    current = driver.current_url

    events = []

    # change dropdown visibility
    element = driver.find_element(By.NAME, "event_id")
    driver.execute_script("arguments[0].style.display = 'block';", element)

    # get option elements
    options = driver.find_elements(By.XPATH, "//select[@name = 'event_id']/option")

    # search name text in options, store options that contain name
    for option in options:
        if name in option.text:
            events.append(option)

    # go through options including name
    for k in range(len(events)):

        current = driver.current_url
        event_name = events[k].text
        events[k].click()

        try:
            driver.find_element(By.PARTIAL_LINK_TEXT, "Prelim Records").click()

            table = pd.read_html(driver.page_source)
            table = table[0]

            # dict key
            x = get_random_string(8)

            debate_partic[x] = {}

            debate_partic[x]["tournament"] = driver.find_element(By.TAG_NAME, "h2").text
            debate_partic[x]["year"] = driver.find_element(By.TAG_NAME, "h5").text
            debate_partic[x]["participants"] = len(table.index)
            debate_partic[x]["event"] = event
            debate_partic[x]["event_name"] = event_name

        except:
            driver.back()
        
        driver.get(current)

        # redefine events
        events = []

        element = driver.find_element(By.NAME, "event_id")
        driver.execute_script("arguments[0].style.display = 'block';", element)

        # get option elements
        options = driver.find_elements(By.XPATH, "//select[@name = 'event_id']/option")

        for option in options:
            if name in option.text:
                events.append(option)

# get the data!
for i in range(len(links)): 
    driver.get(links[i])

    select("LD", "Lincoln")
    select("LD", "LD")
    select("PF", "Public")
    select("PF", "PF")

# turn dict to dataframe
debate_partic = pd.DataFrame.from_dict(debate_partic, orient='index')

debate_partic.to_csv('debate_partic.csv')
