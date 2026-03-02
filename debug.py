import undetected_chromedriver as uc
from undetected_chromedriver import Chrome
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys 
import os
import subprocess
import re
import logging
import asyncio
from multiprocessing import freeze_support

link = "https://www.perplexity.ai/"
driver_path = open("driver-path.txt").read().strip() + "/bin/undetected-chromedriver"
chrome_path = subprocess.check_output(["which", "chromium-browser"]).decode().strip()

class WaitDriver:
    def __init__(self):
        self.driver = uc.Chrome(
            headless=False,
            use_subprocess=False,
            browser_executable_path=chrome_path,
            driver_executable_path=driver_path
        )
        self.waiter = WebDriverWait(self.driver, 25, poll_frequency=0.2)
        self.query_prepend = "Keep answer to 4 sentences. "
        self.save_ctx = True
        self.driver.get(link)
        self.waiter.until(
            EC.element_to_be_clickable((By.CSS_SELECTOR, 'button[aria-label="Close"]'))
        ).click()

    # log erros for these methods later
    def clear_ctx(self) -> None: 
        self.driver.get(link)
        
    def query(self, query: str) -> str:
        query = self.query_prepend + query # configurable later
        close_elements = self.driver.find_elements(By.XPATH, '//div[text()="Close"]')
        if close_elements:
            close_elements[0].click()
        self.driver.find_element(By.ID, "ask-input").send_keys(query + Keys.ENTER)
        self.waiter.until(
            EC.element_to_be_clickable((By.CSS_SELECTOR,'button[aria-label="Helpful"]'))
        )
        elements = self.driver.find_elements(By.CSS_SELECTOR, '[id^="markdown-content-"]')
        return [e.text for e in elements]



async def main() -> None:
    wd = WaitDriver()
    wd.query("history of asmr")    
    e = wd.query("who started doing asmr first")
    import code
    code.interact(local=locals())
    
if __name__ == "__main__":
    freeze_support()
    asyncio.run(main())
