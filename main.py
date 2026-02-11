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
from dataclasses import dataclass

link = "https://www.perplexity.ai/"
logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(name)s: %(message)s" )

driver_path = open("driver-path.txt").read().strip() + "/bin/undetected-chromedriver"
chrome_path = subprocess.check_output(["which", "chromium-browser"]).decode().strip()

def length_translater(opt: str) -> str:
    match opt:
        case "short":
            return "Keep answer to 2 sentences. " 
        case "mid":
            return "Keep answer to 4 sentences. "
        case "long":
            return "Keep answer to 8 sentences. "
        case "uncapped":
            return ""
    

@dataclass
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
        if not self.save_ctx:
            self.clear_ctx()
        
        text = elements[len(elements) - 1].text
        citations_stripped = re.sub(r'\w+\+\d',"", text)
        return citations_stripped


    # mutates waitdriver settings
    def opts_present(self, query: str, peer: str) -> bool:
        if query == ":ctx off":
            self.save_ctx = False
            self.clear_ctx()
            logging.info("Context turned off for %s", peer)
            return True
    
        if query == ":ctx on":
            self.save_ctx = True
            logging.info("Context turned on for %s", peer)
            return True

        if query.startswith(":length"):
            self.query_prepend = length_translater(query.split()[1])
            logging.info("Response length set to %s for %s", self.query_prepend, peer)
            return True            
        return False 

async def handler(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> None:
    addr = writer.get_extra_info("peername")
    logging.info("Connection from %s", addr)
    wd = WaitDriver()
    writer.write("READY".encode()) # literally doesnt matter what string this is 
    await writer.drain()

    try:
        while True:
            query = (await reader.readuntil(b"\r\n\r\n")).decode().strip()
            logging.info("Query/Option received:\n|%s|\n", query)
            if query == ":exit":
                break
            
            result = ""
            if not wd.opts_present(query, addr):
                loop = asyncio.get_event_loop()
                result = await loop.run_in_executor(None, wd.query, query)
            else:
                result = "[*] Option successfully set on the server"
            writer.write(result.encode())
            writer.write(b"\r\n\r\n")
            await writer.drain()
        
    except asyncio.IncompleteReadError:
        logging.info("Client disconnected")
    except Exception as e:
        logging.WARNING("Error communicating with [%s]: %s", addr, e)
    finally:
        logging.info("Closing connection from %s", addr)
        writer.close()
        await writer.wait_closed()
        wd.driver.quit()
        

async def main() -> None:
    host = "0.0.0.0"
    port = 4321
    server = await asyncio.start_server(
       handler,
       host,
       port
    )

    try:
        async with server:
            logging.info("Listening on %s:%d", host, port)
            await server.serve_forever()
    except Exception as e:
        logging.ERROR("Exiting due to error: %s", e)
        return

    
if __name__ == "__main__":
    asyncio.run(main())
