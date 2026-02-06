import undetected_chromedriver as uc
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys 
import os
import subprocess
import re
import argparse

parser = argparse.ArgumentParser(description="Perplexity CLI")
parser.add_argument("query", help="Query to be sent to perplexity")

if __name__ == "__main__":
    args  = parser.parse_args()
    query = "Keep answer to 2 sentences. " + args.query 
    driver_path = open("driver-path.txt").read().strip() + "/bin/undetected-chromedriver"
    chrome_path = subprocess.check_output(["which", "chromium-browser"]).decode().strip()
    
    driver = uc.Chrome(
        headless=False,
        use_subprocess=False,
        browser_executable_path=chrome_path,
        driver_executable_path=driver_path
    )

    wait = WebDriverWait(driver, 25, poll_frequency=0.2)

    driver.get("https://www.perplexity.ai/")

    wait.until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, 'button[aria-label="Close"]'))
    ).click()

    driver.find_element(By.ID, "ask-input").send_keys(query + Keys.ENTER)
    wait.until(
        EC.element_to_be_clickable((By.CSS_SELECTOR,'button[aria-label="Helpful"]'))
    )
    elements = driver.find_elements(By.CSS_SELECTOR, '[id^="markdown-content-"]')
    text = elements[len(elements) - 1].text
    citations_stripped = re.sub(r'\w+\+\d',"", text)
    print( citations_stripped + "\n\n")
    
    driver.quit()
