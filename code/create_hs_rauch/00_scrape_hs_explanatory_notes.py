#!/usr/bin/env python3
"""
Harmonized System Explanatory Notes Scraper
Specialized scraper for WCO Trade Tools HS explanatory notes
Supports multiple revisions and creates nested directory structure
"""

import os
import time
import logging
import re
import argparse
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass

from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from bs4 import BeautifulSoup

# Load environment variables for authentication
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass  # python-dotenv is optional, will use os.environ directly

@dataclass
class HSContent:
    """Data structure for HS content"""
    revision: str
    section_number: str
    chapter_number: Optional[str]
    title: str
    content_html: str
    url: str
    timestamp: str
    content_type: str  # 'section' or 'chapter'


class HSExplanatoryNotesScraper:
    """Specialized scraper for Harmonized System explanatory notes"""
    
    def __init__(self, 
                 base_url: str = "https://www.wcotradetools.org/en/harmonized-system",
                 revisions: List[str] = None,
                 sections: List[str] = None,
                 output_dir: str = "wco_scraped_data",
                 headless: bool = True,
                 delay: float = 2.0,
                 firefox_binary_path: str = None,
                 geckodriver_path: str = None,
                 username: str = None,
                 password: str = None,
                 verbose: bool = False,
                 debug_mode: bool = False):
        
        self.base_url = base_url
        self.revisions = revisions or ["2022"]
        self.sections = sections or [f"{i:02d}" for i in range(1, 22)]  # 01-21
        self.output_dir = Path(output_dir)
        self.headless = headless
        self.delay = delay
        self.firefox_binary_path = firefox_binary_path
        self.geckodriver_path = geckodriver_path
        self.username = username
        self.password = password
        self.verbose = verbose
        self.debug_mode = debug_mode
        self.driver = None
        self.screenshot_dir = None
        
        # Setup logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s',
            filename="./logs/hs_scraper.log"
        )
        self.logger = logging.getLogger(__name__)
        
        # Create output directory
        self.output_dir.mkdir(exist_ok=True)
        
        # Setup screenshot directory for debug mode
        if self.debug_mode:
            self._setup_screenshot_directory()
        
        # Statistics
        self.stats = {
            'sections_processed': 0,
            'chapters_processed': 0,
            'subheadings_skipped': 0,
            'files_created': 0,
            'errors': 0
        }
    
    def _setup_screenshot_directory(self) -> None:
        """Setup screenshot directory in project temp/scrn location for debug mode"""
        try:
            # Use the project's temp/scrn directory
            project_root = Path(__file__).parent.parent
            self.screenshot_dir = project_root / "temp" / "scrn"
            self.screenshot_dir.mkdir(parents=True, exist_ok=True)
            
            self.logger.info(f"Debug mode enabled - screenshots will be saved to: {self.screenshot_dir}")
            
        except Exception as e:
            self.logger.error(f"Failed to setup screenshot directory: {e}")
            self.screenshot_dir = None
    
    def take_screenshot(self, filename: str) -> None:
        """Take a screenshot if debug mode is enabled"""
        if not self.debug_mode or not self.screenshot_dir or not self.driver:
            return
            
        try:
            # Add timestamp to filename for uniqueness
            timestamp = datetime.now().strftime("%H%M%S")
            screenshot_filename = f"{timestamp}_{filename}.png"
            screenshot_path = self.screenshot_dir / screenshot_filename
            
            # Take the screenshot
            self.driver.save_screenshot(str(screenshot_path))
            self.logger.info(f"Screenshot saved: {screenshot_path}")
            
        except Exception as e:
            self.logger.warning(f"Failed to take screenshot '{filename}': {e}")
    
    def _find_firefox_binary(self) -> str:
        """Try to find Firefox binary in common locations"""
        import shutil
        
        # Check if firefox is in PATH
        firefox_path = shutil.which('firefox')
        if firefox_path:
            return firefox_path
        
        # Check common locations
        common_paths = [
            '/usr/bin/firefox',
            '/usr/local/bin/firefox',
            '/opt/firefox/firefox',
            '/snap/bin/firefox',
            '~/firefox/firefox',
            os.path.expanduser('~/bin/firefox'),
            os.path.expanduser('~/.local/bin/firefox')
        ]
        
        for path in common_paths:
            expanded_path = os.path.expanduser(path)
            if os.path.exists(expanded_path):
                return expanded_path
        
        raise FileNotFoundError("Firefox binary not found. Please specify firefox_binary_path")
    
    def _find_geckodriver(self) -> str:
        """Try to find geckodriver"""
        import shutil
        
        # Check if geckodriver is in PATH
        geckodriver_path = shutil.which('geckodriver')
        if geckodriver_path:
            return geckodriver_path
        
        # Check current directory with absolute path
        local_geckodriver = Path('./geckodriver').resolve()
        if local_geckodriver.exists():
            return str(local_geckodriver)
        
        # Check common user locations
        common_paths = [
            './geckodriver',
            '~/geckodriver',
            '~/.local/bin/geckodriver',
            '~/bin/geckodriver'
        ]
        
        for path in common_paths:
            expanded_path = Path(os.path.expanduser(path)).resolve()
            if expanded_path.exists():
                return str(expanded_path)
        
        raise FileNotFoundError("Geckodriver not found. Please download it manually.")
    
    def init_driver(self) -> webdriver.Firefox:
        """Initialize Firefox WebDriver"""
        self.logger.info("Initializing Firefox WebDriver for HS scraping...")
        
        # Set up Firefox options
        options = Options()
        if self.headless:
            options.add_argument('--headless')
        
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        options.add_argument('--disable-gpu')
        options.add_argument('--disable-extensions')
        
        # Set Firefox binary path
        if self.firefox_binary_path:
            options.binary_location = self.firefox_binary_path
        else:
            try:
                options.binary_location = self._find_firefox_binary()
                self.logger.info(f"Found Firefox at: {options.binary_location}")
            except FileNotFoundError as e:
                self.logger.error(str(e))
                raise
        
        # Set up geckodriver service
        if self.geckodriver_path:
            geckodriver_path = str(Path(self.geckodriver_path).resolve())
            service = Service(geckodriver_path)
            self.logger.info(f"Using provided geckodriver at: {geckodriver_path}")
        else:
            try:
                geckodriver_path = self._find_geckodriver()
                service = Service(geckodriver_path)
                self.logger.info(f"Found geckodriver at: {geckodriver_path}")
                
                # Verify the file exists and is executable
                gecko_path = Path(geckodriver_path)
                if not gecko_path.exists():
                    raise FileNotFoundError(f"Geckodriver not found at resolved path: {geckodriver_path}")
                if not os.access(geckodriver_path, os.X_OK):
                    raise PermissionError(f"Geckodriver is not executable: {geckodriver_path}")
                    
            except FileNotFoundError as e:
                self.logger.error(str(e))
                raise
        
        # Create WebDriver instance
        try:
            driver = webdriver.Firefox(service=service, options=options)
            driver.set_page_load_timeout(30)
            driver.implicitly_wait(10)
            return driver
        except Exception as e:
            self.logger.error(f"Failed to initialize Firefox WebDriver: {e}")
            raise
    
    def handle_cookie_consent(self) -> None:
        """Handle cookie consent popup if present"""
        try:
            wait = WebDriverWait(self.driver, 10)
            
            # WCO-specific cookie consent selectors based on the captured HTML
            wco_consent_selectors = [
                "//button[contains(@class, 'agree-button') and contains(@class, 'eu-cookie-compliance-default-button')]",
                "//*[contains(@class, 'agree-button')]",
                "//*[contains(@class, 'eu-cookie-compliance-default-button')]",
                "//button[contains(@class, 'agree-button')]",
                "//*[@id='popup-buttons']//button"
            ]
            
            # Try WCO-specific selectors first
            for selector in wco_consent_selectors:
                try:
                    self.logger.debug(f"Looking for cookie consent with selector: {selector}")
                    consent_button = wait.until(
                        EC.element_to_be_clickable((By.XPATH, selector))
                    )
                    if consent_button and consent_button.is_displayed():
                        self.logger.info(f"Found and clicking cookie consent button: {selector}")
                        consent_button.click()
                        time.sleep(2)  # Wait for popup to disappear
                        return
                except TimeoutException:
                    self.logger.debug(f"Timeout waiting for selector: {selector}")
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with selector {selector}: {e}")
                    continue
            
            # Fallback to generic selectors
            generic_selectors = [
                "//button[contains(@class, 'cookie')]",
                "//button[contains(@class, 'consent')]",
                "//button[contains(@id, 'cookie')]",
                "//*[contains(@class, 'cookie-accept')]",
                "//*[@id='cookie-accept']",
                "//*[@data-cookie='accept']"
            ]
            
            for selector in generic_selectors:
                try:
                    consent_button = wait.until(
                        EC.element_to_be_clickable((By.XPATH, selector))
                    )
                    if consent_button and consent_button.is_displayed():
                        self.logger.info(f"Found cookie consent with generic selector: {selector}")
                        consent_button.click()
                        time.sleep(2)
                        return
                except TimeoutException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with generic selector {selector}: {e}")
                    continue
                    
        except Exception as e:
            self.logger.debug(f"No cookie consent popup found or error: {e}")
        
        # Additional check for popup visibility
        try:
            popup = self.driver.find_element(By.XPATH, "//*[@id='sliding-popup']")
            if popup.is_displayed():
                self.logger.warning("Cookie popup still visible after consent attempts")
        except NoSuchElementException:
            self.logger.debug("No sliding popup found")
    
    def perform_login(self) -> bool:
        """Perform the complete login process"""
        try:
            self.logger.info("Starting login process...")
            
            # Get credentials from instance variables or environment
            username = self.username or os.getenv('WCO_USERNAME')
            password = self.password or os.getenv('WCO_PASSWORD')
            
            if not username or not password:
                self.logger.error("WCO_USERNAME and WCO_PASSWORD must be set in .env file or provided as parameters")
                return False
            
            self.logger.info(f"Using username: {username[:3]}***")
            
            # Step 1: Navigate to homepage
            self.logger.info("Step 1: Navigating to homepage...")
            if not self.navigate_to_page("https://www.wcotradetools.org/"):
                return False
            
            self.take_screenshot("login_step1_homepage")
            
            # Step 2: Click Sign In button
            self.logger.info("Step 2: Looking for Sign In button...")
            sign_in_selectors = [
                "//*[contains(@class, 'btn-login') and contains(@class, 'sign-in')]",
                "//span[contains(@class, 'sign-in')]",
                "//*[contains(@class, 'btn') and contains(@class, 'sign-in')]",
                "//button[contains(@class, 'sign-in')]",
                "//a[contains(@class, 'sign-in')]",
                "//*[contains(@class, 'login')]"
            ]
            
            sign_in_clicked = False
            for selector in sign_in_selectors:
                try:
                    wait = WebDriverWait(self.driver, 5)
                    sign_in_button = wait.until(EC.element_to_be_clickable((By.XPATH, selector)))
                    if sign_in_button and sign_in_button.is_displayed():
                        self.logger.info(f"Found Sign In button with selector: {selector}")
                        sign_in_button.click()
                        sign_in_clicked = True
                        time.sleep(2)  # Wait for dropdown to appear
                        break
                except TimeoutException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with sign-in selector {selector}: {e}")
                    continue
            
            if not sign_in_clicked:
                self.logger.error("Could not find or click Sign In button")
                return False
            
            self.take_screenshot("login_step2_dropdown_appeared")
            
            # Give more time for dropdown/menu to fully appear
            time.sleep(3)
            
            # Step 3: Click Connect button in the dropdown
            self.logger.info("Step 3: Looking for Connect button in dropdown...")
            
            # Comprehensive analysis of what appeared after clicking Sign In
            try:
                # Check for any new elements that might have appeared
                all_links = self.driver.find_elements(By.XPATH, "//a")
                login_related_links = []
                
                for link in all_links:
                    if link.is_displayed():
                        link_text = link.text.strip().lower()
                        link_href = link.get_attribute('href') or ''
                        link_class = link.get_attribute('class') or ''
                        
                        # Look for login-related links
                        if (any(keyword in link_text for keyword in ['connect', 'login', 'sign in', 'log in']) or
                            any(keyword in link_href.lower() for keyword in ['login', 'connect', 'openid']) or
                            any(keyword in link_class.lower() for keyword in ['login', 'connect', 'sign-in'])):
                            
                            login_related_links.append({
                                'text': link_text,
                                'href': link_href,
                                'class': link_class
                            })
                
                self.logger.info(f"Found {len(login_related_links)} login-related links after clicking Sign In:")
                for i, link_info in enumerate(login_related_links[:5], 1):  # Show first 5
                    self.logger.info(f"  {i}. Text: '{link_info['text']}', Href: '{link_info['href']}', Class: '{link_info['class']}'")
                
                # Also check dropdowns/menus
                dropdown_elements = self.driver.find_elements(By.XPATH, "//*[contains(@class, 'dropdown-menu') or contains(@class, 'menu') or contains(@class, 'dropdown')]")
                if dropdown_elements:
                    self.logger.info(f"Found {len(dropdown_elements)} dropdown/menu elements")
                    for i, dropdown in enumerate(dropdown_elements):
                        if dropdown.is_displayed():
                            dropdown_text = dropdown.text.strip()
                            self.logger.debug(f"Dropdown {i+1} content: {dropdown_text[:200]}...")
                else:
                    self.logger.warning("No dropdown elements found - login dropdown may not have appeared")
                    
            except Exception as e:
                self.logger.debug(f"Error checking page elements: {e}")
            
            connect_selectors = [
                "//a[contains(@class, 'btn') and contains(@class, 'sign-in') and contains(@class, 'button')]",
                "//*[contains(@class, 'btn') and contains(@class, 'sign-in')]",
                "//a[contains(@href, '/login')]",
                "//a[contains(@href, 'connect')]",
                "//*[contains(@class, 'openid-connect-login-form')]//a",
                "//a[contains(@class, 'button') and contains(@class, 'js-form-submit')]",
                # Additional fallback selectors
                "//a[contains(@class, 'connect')]",
                "//button[contains(@class, 'connect')]",
                "//a[contains(@onclick, 'login')]",
                "//*[contains(@data-drupal-link-system-path, 'login')]"
            ]
            
            connect_clicked = False
            for selector in connect_selectors:
                try:
                    wait = WebDriverWait(self.driver, 5)
                    connect_button = wait.until(EC.element_to_be_clickable((By.XPATH, selector)))
                    if connect_button and connect_button.is_displayed():
                        connect_text = connect_button.text.strip().lower()
                        button_href = connect_button.get_attribute('href') or ''
                        
                        # More lenient matching - check for 'connect' in text OR if it has a login-related href
                        is_connect_button = (
                            'connect' in connect_text or 
                            '/login' in button_href or 
                            'openid' in button_href.lower() or
                            connect_button.get_attribute('class') and 'sign-in' in connect_button.get_attribute('class')
                        )
                        
                        if is_connect_button:
                            self.logger.info(f"Found Connect button: '{connect_text}' with selector: {selector}, href: {button_href}")
                            
                            # Scroll button into view first
                            self.driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", connect_button)
                            time.sleep(3)
                            
                            # Store current URL to verify navigation
                            current_url_before = self.driver.current_url
                            
                            # Try JavaScript click if regular click fails
                            click_successful = False
                            try:
                                connect_button.click()
                                self.logger.info("Regular click executed")
                                click_successful = True
                            except Exception as click_error:
                                self.logger.warning(f"Regular click failed: {click_error}, trying JavaScript click...")
                                try:
                                    self.driver.execute_script("arguments[0].click();", connect_button)
                                    self.logger.info("JavaScript click executed")
                                    click_successful = True
                                except Exception as js_error:
                                    self.logger.error(f"Both clicks failed: {js_error}")
                                    continue
                            
                            if click_successful:
                                # Wait and check if URL changed or login form appeared
                                time.sleep(3)
                                current_url_after = self.driver.current_url
                                
                                # Check if we navigated to a different page or if login form appeared
                                url_changed = current_url_before != current_url_after
                                login_form_present = len(self.driver.find_elements(By.XPATH, 
                                    "//input[@type='password' or contains(@name, 'password') or contains(@name, 'username') or contains(@name, 'email')]")) > 0
                                
                                if url_changed or login_form_present:
                                    self.logger.info(f"✅ Connect click successful - URL changed: {url_changed}, Login form: {login_form_present}")
                                    connect_clicked = True
                                    break
                                else:
                                    self.logger.warning(f"⚠️ Click executed but no navigation detected. URL before: {current_url_before}, after: {current_url_after}")
                                    # Wait a bit longer and try again with next selector
                                    time.sleep(2)
                                    continue
                        else:
                            self.logger.debug(f"Button doesn't match connect criteria: text='{connect_text}', href='{button_href}'")
                            
                except TimeoutException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with connect selector {selector}: {e}")
                    continue
            
            if not connect_clicked:
                self.logger.warning("Could not find or click Connect button, trying direct navigation to login...")
                # Take debug screenshot to see current state
                self.take_screenshot("login_step3_connect_failed")
                
                # Try direct navigation to login URL as fallback
                login_urls = [
                    "https://www.wcotradetools.org/user/login",
                    "https://www.wcotradetools.org/login",
                    "https://www.wcotradetools.org/openid_connect/generic"
                ]
                
                navigation_success = False
                for login_url in login_urls:
                    try:
                        self.logger.info(f"Trying direct navigation to: {login_url}")
                        self.driver.get(login_url)
                        time.sleep(3)
                        
                        # Check if we're on a login page by looking for login form elements
                        login_indicators = self.driver.find_elements(By.XPATH, 
                            "//input[@type='password' or contains(@name, 'password') or contains(@name, 'username') or contains(@name, 'email')]")
                        
                        if login_indicators:
                            self.logger.info(f"✅ Successfully navigated to login page: {login_url}")
                            navigation_success = True
                            break
                        else:
                            self.logger.debug(f"No login form found at {login_url}")
                            
                    except Exception as e:
                        self.logger.debug(f"Failed to navigate to {login_url}: {e}")
                        continue
                
                if not navigation_success:
                    self.logger.error("❌ All login navigation attempts failed")
                    return False
            
            # Verify we're on a login page or being redirected
            current_url = self.driver.current_url
            self.logger.info(f"After clicking Connect, current URL: {current_url}")
            
            # Check for any loading indicators or overlays
            try:
                loading_indicators = self.driver.find_elements(By.XPATH, "//*[contains(@class, 'loading') or contains(@class, 'spinner')]")
                if loading_indicators:
                    self.logger.info("Found loading indicators, waiting for page to load...")
                    time.sleep(5)
            except:
                pass
            
            self.take_screenshot("login_step3_redirected_to_login")
            
            # Step 4: Fill in login credentials
            self.logger.info("Step 4: Filling in login credentials...")
            
            # Wait for login form to load
            time.sleep(3)
            
            # Find username field
            username_selectors = [
                "//input[@name='username']",
                "//input[@name='email']",
                "//input[@name='user']",
                "//input[@type='email']",
                "//input[contains(@id, 'username')]",
                "//input[contains(@id, 'email')]",
                "//input[contains(@placeholder, 'username')]",
                "//input[contains(@placeholder, 'email')]"
            ]
            
            username_filled = False
            for selector in username_selectors:
                try:
                    username_field = self.driver.find_element(By.XPATH, selector)
                    if username_field and username_field.is_displayed():
                        self.logger.info(f"Found username field: {selector}")
                        username_field.clear()
                        username_field.send_keys(username)
                        username_filled = True
                        break
                except NoSuchElementException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with username selector {selector}: {e}")
                    continue
            
            if not username_filled:
                self.logger.error("Could not find username field")
                return False
            
            # Find password field
            password_selectors = [
                "//input[@name='password']",
                "//input[@type='password']",
                "//input[contains(@id, 'password')]",
                "//input[contains(@placeholder, 'password')]"
            ]
            
            password_filled = False
            for selector in password_selectors:
                try:
                    password_field = self.driver.find_element(By.XPATH, selector)
                    if password_field and password_field.is_displayed():
                        self.logger.info(f"Found password field: {selector}")
                        password_field.clear()
                        password_field.send_keys(password)
                        password_filled = True
                        break
                except NoSuchElementException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with password selector {selector}: {e}")
                    continue
            
            if not password_filled:
                self.logger.error("Could not find password field")
                return False
            
            self.take_screenshot("login_step4_credentials_filled")
            
            # Step 5: Submit login form
            self.logger.info("Step 5: Submitting login form...")
            submit_selectors = [
                "//button[@type='submit']",
                "//input[@type='submit']",
                "//button[@name='submit']",
                "//*[contains(@class, 'btn') and @type='submit']",
                "//button[contains(@class, 'btn-primary')]",
                "//input[contains(@class, 'btn-primary')]"
            ]
            
            login_submitted = False
            for selector in submit_selectors:
                try:
                    submit_button = self.driver.find_element(By.XPATH, selector)
                    if submit_button and submit_button.is_displayed():
                        self.logger.info(f"Found submit button: {selector}")
                        submit_button.click()
                        login_submitted = True
                        break
                except NoSuchElementException:
                    continue
                except Exception as e:
                    self.logger.debug(f"Error with submit selector {selector}: {e}")
                    continue
            
            if not login_submitted:
                self.logger.error("Could not find submit button")
                return False
            
            # Wait for login to complete
            time.sleep(5)
            self.take_screenshot("login_step5_after_submit")
            
            # Step 6: Verify login success
            self.logger.info("Step 6: Verifying login success...")
            
            # Check if we're back on the main site and logged in
            current_url = self.driver.current_url
            self.logger.info(f"Current URL after login: {current_url}")
            
            # Look for indicators of successful login
            login_success_indicators = [
                "//*[contains(@class, 'user-menu')]",
                "//*[contains(@class, 'logged-in')]",
                "//*[contains(@class, 'user-profile')]",
                "//a[contains(@href, 'logout')]",
                "//*[contains(@class, 'user-name')]"
            ]
            
            logged_in = False
            for indicator in login_success_indicators:
                try:
                    element = self.driver.find_element(By.XPATH, indicator)
                    if element and element.is_displayed():
                        self.logger.info(f"Login success indicator found: {indicator}")
                        logged_in = True
                        break
                except NoSuchElementException:
                    continue
            
            # Alternative check: see if we can access restricted content
            if not logged_in:
                # Try to navigate to a HS section and see if we get more content
                test_url = f"{self.base_url}/2022/en/01"
                self.driver.get(test_url)
                time.sleep(3)
                
                # Look for content that would only be available when logged in
                restricted_content = self.driver.find_elements(By.XPATH, "//*[contains(@class, 'explanatory-notes') or contains(@class, 'notes')]")
                if len(restricted_content) > 0:
                    logged_in = True
                    self.logger.info("Login success verified by accessing restricted content")
            
            self.take_screenshot("login_step6_verification")
            
            if logged_in:
                self.logger.info("✅ Login successful!")
                return True
            else:
                self.logger.error("❌ Login verification failed")
                return False
                
        except Exception as e:
            self.logger.error(f"Error during login process: {e}")
            self.take_screenshot("login_error")
            return False
    
    def navigate_to_page(self, url: str) -> bool:
        """Navigate to a specific page with error handling"""
        try:
            self.logger.info(f"Navigating to: {url}")
            self.driver.get(url)
            
            # Wait for page to load
            WebDriverWait(self.driver, 15).until(
                EC.presence_of_element_located((By.XPATH, "//body"))
            )
            
            # Handle cookie consent first
            self.handle_cookie_consent()
            
            # Wait for content to load after cookie consent
            time.sleep(3)
            
            # Check if we're on the expected page by looking for HS-specific content
            try:
                # Look for HS-specific elements that should be on section pages
                hs_indicators = [
                    "//h1 | //h2",  # Main headings
                    "//*[contains(@class, 'section-title')]",  # Section titles
                    "//*[contains(@data-drupal-link-system-path, 'harmonized')]",  # HS links
                    "//*[contains(@class, 'hs-section')]",  # HS section content
                    "//main | //*[contains(@class, 'main-content')]"  # Main content areas
                ]
                
                content_found = False
                for indicator in hs_indicators:
                    try:
                        elements = self.driver.find_elements(By.XPATH, indicator)
                        if elements:
                            content_found = True
                            self.logger.debug(f"Found HS content indicator: {indicator}")
                            break
                    except NoSuchElementException:
                        continue
                
                if not content_found:
                    self.logger.warning(f"No HS-specific content found on {url}")
                
            except Exception as e:
                self.logger.warning(f"Could not verify HS content on {url}: {e}")
            
            return True
        except Exception as e:
            self.logger.error(f"Failed to navigate to {url}: {e}")
            return False
    
    def scrape_section_content(self, section: str, revision: str = "2022") -> bool:
        """Scrape all content for a given section from a single page"""
        try:
            section_url = f"{self.base_url}/{revision}/en/{section}"
            self.logger.info(f"Starting to scrape section {section}")
            
            if not self.navigate_to_page(section_url):
                self.logger.error(f"Failed to navigate to section {section}")
                return False
            
            self.take_screenshot(f"section_{section}_overview")
            
            # Get all chapters and headings from this single page
            content_structure = self.get_section_structure()
            if not content_structure:
                self.logger.warning(f"No content structure found in section {section}")
                return False
            
            # Process each chapter and its headings
            for chapter_info in content_structure:
                chapter_num = chapter_info['chapter']
                self.logger.info(f"Processing chapter {chapter_num} in section {section}")
                
                # Scrape chapter-level content
                if not self.scrape_chapter_content_on_page(section, chapter_num, chapter_info, revision):
                    self.logger.error(f"Failed to scrape chapter {chapter_num}")
                    continue
                
                # Process headings within this chapter
                for heading_info in chapter_info['headings']:
                    heading_num = heading_info['heading']
                    self.logger.info(f"Processing heading {heading_num} in chapter {chapter_num}")
                    
                    if not self.scrape_heading_content_on_page(section, chapter_num, heading_num, heading_info, revision):
                        self.logger.warning(f"Failed to scrape heading {heading_num}")
                        continue
                    
                    # Collapse heading after scraping to clean up page
                    self.collapse_nomenclature_item(heading_info['element'], f"heading_{heading_num}")
                    
                    time.sleep(self.delay / 3)  # Short delay between headings
                
                # Collapse chapter after processing all its headings
                self.collapse_nomenclature_item(chapter_info['element'], f"chapter_{chapter_num}")
                
                self.stats['chapters_processed'] += 1
                time.sleep(self.delay / 2)  # Short delay between chapters
            
            self.stats['sections_processed'] += 1
            return True
            
        except Exception as e:
            self.logger.error(f"Error scraping section {section}: {e}")
            self.stats['errors'] += 1
            return False
    
    def get_section_structure(self) -> List[Dict]:
        """Extract complete chapter and heading structure from the current section page"""
        try:
            content_structure = []
                        
            self.logger.info("Waiting for nomenclature items to load...")
            wait = WebDriverWait(self.driver, 15)
            
            # Wait for at least one nomenclature item to be present
            wait.until(EC.presence_of_element_located((By.XPATH, "//div[contains(@class, 'nomenclature-item')]")))
            time.sleep(3)  # Additional wait for all items to load
            
            # Find all nomenclature items (chapters and headings)
            nomenclature_items = self.driver.find_elements(By.XPATH, 
                "//div[contains(@class, 'nomenclature-item')]")
            
            self.logger.info(f"Found {len(nomenclature_items)} total nomenclature items")
            
            if not nomenclature_items:
                self.logger.warning("No nomenclature items found")
                return []
            
            current_chapter = None
            chapters_found = 0
            headings_found = 0
            
            for i, item in enumerate(nomenclature_items):
                try:
                    # Check the data-nomenclature-type attribute
                    nomenclature_type = item.get_attribute('data-nomenclature-type')
                    wco_hs_code = item.get_attribute('data-wco-hs-code')
                    
                    self.logger.debug(f"Item {i}: type={nomenclature_type}, code={wco_hs_code}")
                    
                    if nomenclature_type == '2':  # Chapter
                        chapters_found += 1
                        # Extract chapter number and info
                        chapter_num = self.extract_chapter_number_from_element(item)
                        if chapter_num:
                            current_chapter = {
                                'chapter': chapter_num,
                                'element': item,
                                'headings': [],
                                'wco_hs_code': wco_hs_code
                            }
                            content_structure.append(current_chapter)
                            self.logger.info(f"Found chapter: {chapter_num}")
                            
                            # Expand chapter to reveal headings
                            if self.expand_nomenclature_item(item, f"chapter_{chapter_num}"):
                                # Re-scan for headings now that chapter is expanded
                                time.sleep(2)  # Wait for expansion to complete
                                self.logger.info(f"Scanning for headings in expanded chapter {chapter_num}")
                                
                                # Look for headings within this expanded chapter
                                expanded_headings = self.find_headings_in_expanded_chapter(item, chapter_num)
                                current_chapter['headings'].extend(expanded_headings)
                                headings_found += len(expanded_headings)
                    
                    elif nomenclature_type == '3' and current_chapter:  # Heading
                        headings_found += 1
                        # Extract heading number and info
                        heading_num = self.extract_heading_number_from_element(item)
                        if heading_num:
                            heading_info = {
                                'heading': heading_num,
                                'element': item,
                                'wco_hs_code': wco_hs_code
                            }
                            current_chapter['headings'].append(heading_info)
                            self.logger.info(f"Found heading: {heading_num} in chapter {current_chapter['chapter']}")
                
                except Exception as e:
                    self.logger.warning(f"Error processing nomenclature item {i}: {e}")
                    continue
            
            self.logger.info(f"Total found: {chapters_found} chapters, {headings_found} headings")
            self.logger.info(f"Structured into {len(content_structure)} chapters with headings")
            
            for chapter in content_structure:
                self.logger.info(f"Chapter {chapter['chapter']}: {len(chapter['headings'])} headings")
            
            return content_structure
            
        except Exception as e:
            self.logger.error(f"Error getting section structure: {e}")
            return []
    
    def extract_chapter_number_from_element(self, element) -> str:
        """Extract chapter number from chapter element"""
        try:
            # Look for section number span
            section_number = element.find_element(By.XPATH, ".//span[@class='section-number']")
            text = section_number.text.strip()
            
            # Extract number from text like "Chapter 1"
            match = re.search(r'Chapter\s+(\d+)', text, re.IGNORECASE)
            if match:
                return match.group(1).zfill(2)
            
            return None
        except:
            return None
    
    def extract_heading_number_from_element(self, element) -> str:
        """Extract heading number from heading element"""
        try:
            # Look for section number span
            section_number = element.find_element(By.XPATH, ".//span[@class='section-number']")
            text = section_number.text.strip()
            
            # Extract number from text like "Heading 0101"
            match = re.search(r'Heading\s+(\d+)', text, re.IGNORECASE)
            if match:
                return match.group(1)
            
            return None
        except:
            return None
    
    def expand_nomenclature_item(self, element, context: str) -> bool:
        """Expand a nomenclature item (chapter or heading) to reveal its content"""
        try:
            # Check if already expanded
            try:
                sub_items_holder = element.find_element(By.XPATH, ".//div[contains(@class, 'sub-items-holder')]")
                if 'sectionIsOpend' in sub_items_holder.get_attribute('class'):
                    self.logger.debug(f"{context} is already expanded")
                    return True
            except NoSuchElementException:
                self.logger.debug(f"No sub-items-holder found for {context}, may not be expandable")
                return False
            
            # Find the section opener (toggle button)
            section_opener = None
            opener_selectors = [
                ".//span[@class='section-opener']",
                ".//div[@class='item-header']",
                ".//div[contains(@class, 'nomenclature-item-header')]"
            ]
            
            for selector in opener_selectors:
                try:
                    section_opener = element.find_element(By.XPATH, selector)
                    break
                except NoSuchElementException:
                    continue
            
            if not section_opener:
                self.logger.warning(f"Could not find section opener for {context}")
                return False
            
            self.logger.info(f"Expanding {context}")
            
            # Scroll to element and ensure it's visible
            self.driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", section_opener)
            time.sleep(1)
            
            # Wait for element to be clickable
            wait = WebDriverWait(self.driver, 15)
            wait.until(EC.element_to_be_clickable(section_opener))
            
            # Click to expand
            self.driver.execute_script("arguments[0].click();", section_opener)
            self.take_screenshot(f"expanded_{context}")
            
            # Wait for expansion animation to complete
            time.sleep(3)
            
            # Verify expansion worked with retry logic
            max_attempts = 3
            for attempt in range(max_attempts):
                try:
                    sub_items_holder = element.find_element(By.XPATH, ".//div[contains(@class, 'sub-items-holder')]")
                    if 'sectionIsOpend' in sub_items_holder.get_attribute('class'):
                        self.logger.info(f"Successfully expanded {context}")
                        return True
                    else:
                        if attempt < max_attempts - 1:
                            self.logger.debug(f"Expansion verification failed for {context}, retry {attempt + 1}")
                            time.sleep(2)
                        else:
                            self.logger.warning(f"Expansion verification failed for {context} after {max_attempts} attempts")
                except Exception as verify_error:
                    self.logger.debug(f"Error verifying expansion for {context}: {verify_error}")
                    if attempt < max_attempts - 1:
                        time.sleep(2)
            
            return False
                
        except Exception as e:
            self.logger.warning(f"Could not expand {context}: {e}")
            return False
    
    def collapse_nomenclature_item(self, element, context: str) -> bool:
        """Collapse a nomenclature item to clean up the page"""
        try:
            # Check if currently expanded
            try:
                sub_items_holder = element.find_element(By.XPATH, ".//div[contains(@class, 'sub-items-holder')]")
                if 'sectionIsOpend' not in sub_items_holder.get_attribute('class'):
                    self.logger.debug(f"{context} is already collapsed")
                    return True
            except NoSuchElementException:
                self.logger.debug(f"No sub-items-holder found for {context}, may not be collapsible")
                return True  # Consider it "successfully collapsed" if no holder exists
            
            # Find the section opener (toggle button)
            section_opener = None
            opener_selectors = [
                ".//span[@class='section-opener']",
                ".//div[@class='item-header']",
                ".//div[contains(@class, 'nomenclature-item-header')]"
            ]
            
            for selector in opener_selectors:
                try:
                    section_opener = element.find_element(By.XPATH, selector)
                    break
                except NoSuchElementException:
                    continue
            
            if not section_opener:
                self.logger.debug(f"Could not find section opener for {context} collapse")
                return False
            
            self.logger.debug(f"Collapsing {context}")
            
            # Click to collapse
            self.driver.execute_script("arguments[0].click();", section_opener)
            time.sleep(2)  # Wait for collapse animation
            
            return True
            
        except Exception as e:
            self.logger.debug(f"Could not collapse {context}: {e}")
            return False
    
    def find_headings_in_expanded_chapter(self, chapter_element, chapter_num: str) -> List[Dict]:
        """Find all headings within an expanded chapter element"""
        try:
            headings = []
            
            # Look for sub-items within the expanded chapter
            sub_items_holder = chapter_element.find_element(By.XPATH, ".//div[contains(@class, 'sub-items-holder')]")
            
            # Find heading elements within the sub-items holder
            heading_elements = sub_items_holder.find_elements(By.XPATH, 
                ".//div[contains(@class, 'nomenclature-item') and @data-nomenclature-type='3']")
            
            self.logger.debug(f"Found {len(heading_elements)} heading elements in chapter {chapter_num}")
            
            for heading_element in heading_elements:
                try:
                    # Extract heading number
                    heading_num = self.extract_heading_number_from_element(heading_element)
                    wco_hs_code = heading_element.get_attribute('data-wco-hs-code')
                    
                    if heading_num:
                        heading_info = {
                            'heading': heading_num,
                            'element': heading_element,
                            'wco_hs_code': wco_hs_code
                        }
                        headings.append(heading_info)
                        self.logger.debug(f"Found heading {heading_num} in chapter {chapter_num}")
                    
                except Exception as e:
                    self.logger.warning(f"Error processing heading element in chapter {chapter_num}: {e}")
                    continue
            
            return headings
            
        except Exception as e:
            self.logger.warning(f"Error finding headings in expanded chapter {chapter_num}: {e}")
            return []
    
    
    def scrape_chapter_content_on_page(self, section: str, chapter: str, chapter_info: Dict, revision: str = "2022") -> bool:
        """Scrape legal and explanatory notes for a specific chapter on the current page"""
        try:
            self.logger.info(f"Scraping chapter {chapter} content from current page")
            
            # Create directory structure with revision
            chapter_dir = self.output_dir / f"revision_{revision}" / f"section_{section}" / f"chapter_{chapter}"
            chapter_dir.mkdir(parents=True, exist_ok=True)
            
            chapter_element = chapter_info['element']
            
            # Scrape legal notes for this chapter
            legal_notes = self.scrape_legal_notes_for_element(chapter_element, f"chapter_{chapter}")
            if legal_notes:
                legal_file = chapter_dir / "legal_notes.txt"
                self.save_content_to_file(legal_notes, legal_file)
                self.logger.info(f"Saved chapter {chapter} legal notes to {legal_file}")
            
            # Scrape explanatory notes for this chapter
            explanatory_notes = self.scrape_explanatory_notes_for_element(chapter_element, f"chapter_{chapter}")
            if explanatory_notes:
                explanatory_file = chapter_dir / "explanatory_notes.txt"
                self.save_content_to_file(explanatory_notes, explanatory_file)
                self.logger.info(f"Saved chapter {chapter} explanatory notes to {explanatory_file}")
            
            return True
            
        except Exception as e:
            self.logger.error(f"Error scraping chapter {section}/{chapter}: {e}")
            return False
    
    def scrape_heading_content_on_page(self, section: str, chapter: str, heading: str, heading_info: Dict, revision: str = "2022") -> bool:
        """Scrape legal and explanatory notes for a specific heading on the current page"""
        try:
            self.logger.info(f"Scraping heading {heading} content from current page")
            
            # Create directory structure with revision
            heading_dir = self.output_dir / f"revision_{revision}" / f"section_{section}" / f"chapter_{chapter}" / f"heading_{heading}"
            heading_dir.mkdir(parents=True, exist_ok=True)
            
            heading_element = heading_info['element']
            
            # Scrape legal notes for this heading
            legal_notes = self.scrape_legal_notes_for_element(heading_element, f"heading_{heading}")
            if legal_notes:
                legal_file = heading_dir / "legal_notes.txt"
                self.save_content_to_file(legal_notes, legal_file)
                self.logger.info(f"Saved heading {heading} legal notes to {legal_file}")
            
            # Scrape explanatory notes for this heading
            explanatory_notes = self.scrape_explanatory_notes_for_element(heading_element, f"heading_{heading}")
            if explanatory_notes:
                explanatory_file = heading_dir / "explanatory_notes.txt"
                self.save_content_to_file(explanatory_notes, explanatory_file)
                self.logger.info(f"Saved heading {heading} explanatory notes to {explanatory_file}")
            
            return True
            
        except Exception as e:
            self.logger.error(f"Error scraping heading {section}/{chapter}/{heading}: {e}")
            return False
    
    def scrape_legal_notes_for_element(self, element, context: str) -> Optional[str]:
        """Scrape legal notes associated with a specific element (chapter or heading)"""
        try:
            # If this is a heading, ensure it's expanded first to access tabs
            if 'heading_' in context:
                self.logger.info(f"Expanding {context} to access tabs")
                if not self.expand_nomenclature_item(element, context):
                    self.logger.warning(f"Could not expand {context} for legal notes access")
                    return None
            
            # Find the legal notes tab link
            legal_tab = element.find_element(By.XPATH, ".//a[@id='hs-legal-notes-link']")
            
            # Check if the tab is disabled
            tab_classes = legal_tab.get_attribute('class') or ''
            if 'disabled' in tab_classes:
                self.logger.debug(f"Legal notes tab disabled for {context}")
                return None
            
            self.logger.info(f"Clicking legal notes tab for {context}")
            
            # Scroll to element and wait for it to be clickable
            self.driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", legal_tab)
            
            wait = WebDriverWait(self.driver, 10)
            wait.until(EC.element_to_be_clickable(legal_tab))
            
            # Click on legal notes tab
            self.driver.execute_script("arguments[0].click();", legal_tab)
            self.take_screenshot(f"legal_notes_clicked_{context}")
            
            # Wait for content to load
            time.sleep(3)
            
            # Look for the content in the tab block
            try:
                legal_content_block = element.find_element(By.XPATH, 
                    ".//div[@data-tab-content='1' and contains(@class, 'hs-legal-notes')]")
                
                # Wait for content to be visible
                wait.until(EC.visibility_of(legal_content_block))
                
                # Check if this is a freemium section (login required)
                freemium_sections = legal_content_block.find_elements(By.XPATH, 
                    ".//div[contains(@class, 'freemium-section')]")
                
                if freemium_sections:
                    self.logger.info(f"Legal notes require login for {context}")
                    return "Login required to access legal notes content"
                
                # Extract actual content from text-block
                text_blocks = legal_content_block.find_elements(By.XPATH, 
                    ".//div[contains(@class, 'text-block')]")
                
                if text_blocks:
                    content_parts = []
                    for block in text_blocks:
                        block_text = block.text.strip()
                        if block_text:
                            content_parts.append(block_text)
                    
                    if content_parts:
                        content = "\n\n".join(content_parts)
                        self.logger.info(f"Extracted legal notes for {context}: {len(content)} characters")
                        return content
                
                self.logger.debug(f"No text content found in legal notes for {context}")
                return None
                
            except Exception as content_error:
                self.logger.warning(f"Error extracting legal notes content for {context}: {content_error}")
                return None
            
        except Exception as e:
            self.logger.debug(f"No legal notes tab found for {context}: {e}")
            return None
    
    def scrape_explanatory_notes_for_element(self, element, context: str) -> Optional[str]:
        """Scrape explanatory notes associated with a specific element (chapter or heading)"""
        try:
            # If this is a heading, ensure it's expanded first to access tabs
            if 'heading_' in context:
                self.logger.info(f"Expanding {context} to access tabs")
                if not self.expand_nomenclature_item(element, context):
                    self.logger.warning(f"Could not expand {context} for explanatory notes access")
                    return None
            
            # Find the explanatory notes tab link
            explanatory_tab = element.find_element(By.XPATH, ".//a[@id='hs-explanatory-notes-link']")
            
            # Check if the tab is disabled
            tab_classes = explanatory_tab.get_attribute('class') or ''
            if 'disabled' in tab_classes:
                self.logger.debug(f"Explanatory notes tab disabled for {context}")
                return None
            
            self.logger.info(f"Clicking explanatory notes tab for {context}")
            
            # Scroll to element and wait for it to be clickable
            self.driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", explanatory_tab)
            
            wait = WebDriverWait(self.driver, 10)
            wait.until(EC.element_to_be_clickable(explanatory_tab))
            
            # Click on explanatory notes tab
            self.driver.execute_script("arguments[0].click();", explanatory_tab)
            self.take_screenshot(f"explanatory_notes_clicked_{context}")
            
            # Wait for content to load
            time.sleep(3)
            
            # Look for the content in the tab block
            try:
                explanatory_content_block = element.find_element(By.XPATH, 
                    ".//div[@data-tab-content='2' and contains(@class, 'hs-explanatory-notes')]")
                
                # Wait for content to be visible
                wait.until(EC.visibility_of(explanatory_content_block))
                
                # Check if this is a freemium section (login required)
                freemium_sections = explanatory_content_block.find_elements(By.XPATH, 
                    ".//div[contains(@class, 'freemium-section')]")
                
                if freemium_sections:
                    self.logger.info(f"Explanatory notes require login for {context}")
                    return "Login required to access explanatory notes content"
                
                # Extract actual content from text-block
                text_blocks = explanatory_content_block.find_elements(By.XPATH, 
                    ".//div[contains(@class, 'text-block')]")
                
                if text_blocks:
                    content_parts = []
                    for block in text_blocks:
                        block_text = block.text.strip()
                        if block_text:
                            content_parts.append(block_text)
                    
                    if content_parts:
                        content = "\n\n".join(content_parts)
                        self.logger.info(f"Extracted explanatory notes for {context}: {len(content)} characters")
                        return content
                
                self.logger.debug(f"No text content found in explanatory notes for {context}")
                return None
                
            except Exception as content_error:
                self.logger.warning(f"Error extracting explanatory notes content for {context}: {content_error}")
                return None
            
        except Exception as e:
            self.logger.debug(f"No explanatory notes tab found for {context}: {e}")
            return None
    
    def extract_notes_content(self, context: str) -> Optional[str]:
        """Extract content from opened notes modal or expanded content"""
        try:
            # Look for content in various possible containers
            content_selectors = [
                "//*[contains(@class, 'modal-content')]//p",
                "//*[contains(@class, 'popup-content')]//p",
                "//*[contains(@class, 'notes-content')]//p",
                "//*[contains(@class, 'legal-content')]//p",
                "//*[contains(@class, 'explanatory-content')]//p",
                "//*[contains(@id, 'legal')]//p",
                "//*[contains(@id, 'explanatory')]//p",
                "//div[contains(@class, 'content')]//p",
                "//div[@role='dialog']//p",
                "//*[@aria-expanded='true']//p"
            ]
            
            content = ""
            for selector in content_selectors:
                try:
                    elements = self.driver.find_elements(By.XPATH, selector)
                    if elements:
                        content_parts = []
                        for elem in elements:
                            text = elem.text.strip()
                            if text and len(text) > 10:  # Filter out short/empty content
                                content_parts.append(text)
                        
                        if content_parts:
                            content = "\n\n".join(content_parts)
                            self.logger.info(f"Extracted content for {context}: {len(content)} characters")
                            break
                except:
                    continue
            
            # Close any modal/popup that might be open
            self.close_modal_if_open()
            
            return content if content else None
            
        except Exception as e:
            self.logger.error(f"Error extracting content for {context}: {e}")
            return None
    
    def close_modal_if_open(self):
        """Close any open modals or popups"""
        try:
            # Try various ways to close modals
            close_selectors = [
                "//button[contains(@class, 'close')]",
                "//button[@aria-label='Close']",
                "//*[contains(@class, 'modal-close')]",
                "//button[contains(text(), 'Close')]",
                "//button[contains(text(), '×')]",
                "//*[@role='button'][contains(@class, 'close')]"
            ]
            
            for selector in close_selectors:
                try:
                    close_buttons = self.driver.find_elements(By.XPATH, selector)
                    if close_buttons:
                        self.driver.execute_script("arguments[0].click();", close_buttons[0])
                        time.sleep(1)
                        break
                except:
                    continue
            
            # Try pressing Escape key as fallback
            try:
                from selenium.webdriver.common.keys import Keys
                self.driver.find_element(By.TAG_NAME, 'body').send_keys(Keys.ESCAPE)
                time.sleep(1)
            except:
                pass
                
        except Exception as e:
            self.logger.debug(f"Error closing modal: {e}")
    
    
    def save_content_to_file(self, content: str, file_path: Path) -> None:
        """Save scraped content to a file"""
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            self.stats['files_created'] += 1
            self.logger.debug(f"Saved content to {file_path}")
        except Exception as e:
            self.logger.error(f"Error saving content to {file_path}: {e}")
            self.stats['errors'] += 1
    
    def run_full_scrape(self) -> Dict:
        """Run the complete scraping process for all specified sections"""
        self.logger.info("Starting full scrape process")
        
        start_time = time.time()
        
        for revision in self.revisions:
            self.logger.info(f"Processing revision {revision}")
            
            for section in self.sections:
                self.logger.info(f"Processing section {section} for revision {revision}")
                
                if not self.scrape_section_content(section, revision):
                    self.logger.error(f"Failed to scrape section {section}")
                    continue
                
                time.sleep(self.delay)
        
        end_time = time.time()
        duration = end_time - start_time
        
        self.stats['duration_seconds'] = duration
        self.logger.info(f"Scraping completed in {duration:.2f} seconds")
        self.logger.info(f"Final stats: {self.stats}")
        
        return self.stats


def main():
    """Main function to run the HS scraper with command line arguments"""
    parser = argparse.ArgumentParser(
        description="Harmonized System Explanatory Notes Scraper",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python scrape_hs_explanatory_notes.py --debug
  python scrape_hs_explanatory_notes.py --revision 2022 --sections 01,02,03 --debug
  python scrape_hs_explanatory_notes.py --username myuser --password mypass --debug
        """
    )
    
    parser.add_argument('--base-url', 
                       default='https://www.wcotradetools.org/en/harmonized-system',
                       help='Base URL for WCO Trade Tools (default: %(default)s)')
    
    parser.add_argument('--revisions', 
                       default='2022',
                       help='Comma-separated list of HS revisions to scrape (default: %(default)s)')
    
    parser.add_argument('--sections',
                       default='01-21',
                       help='Sections to scrape, e.g., "01,02,03" or "01-21" for all (default: %(default)s)')
    
    parser.add_argument('--output-dir',
                       default='wco_scraped_data',
                       help='Output directory for scraped data (default: %(default)s)')
    
    parser.add_argument('--headless',
                       action='store_true',
                       default=True,
                       help='Run browser in headless mode (default: %(default)s)')
    
    parser.add_argument('--no-headless',
                       action='store_false',
                       dest='headless',
                       help='Run browser with GUI (opposite of --headless)')
    
    parser.add_argument('--delay',
                       type=float,
                       default=2.0,
                       help='Delay between requests in seconds (default: %(default)s)')
    
    parser.add_argument('--firefox-binary',
                       help='Path to Firefox binary (auto-detected if not provided)')
    
    parser.add_argument('--geckodriver',
                       help='Path to geckodriver binary (auto-detected if not provided)')
    
    parser.add_argument('--username',
                       help='Username for WCO login (can also be set via USERNAME env var)')
    
    parser.add_argument('--password',
                       help='Password for WCO login (can also be set via PASSWORD env var)')
    
    parser.add_argument('--verbose', '-v',
                       action='store_true',
                       help='Enable verbose logging')
    
    parser.add_argument('--debug', '-d',
                       action='store_true',
                       help='Enable debug mode with screenshots during login process')
    
    args = parser.parse_args()
    
    # Parse revisions
    if ',' in args.revisions:
        revisions = [r.strip() for r in args.revisions.split(',')]
    else:
        revisions = [args.revisions.strip()]
    
    # Parse sections
    if args.sections == '01-21':
        sections = [f"{i:02d}" for i in range(1, 22)]
    elif ',' in args.sections:
        sections = [s.strip() for s in args.sections.split(',')]
    elif '-' in args.sections:
        start, end = args.sections.split('-')
        sections = [f"{i:02d}" for i in range(int(start), int(end) + 1)]
    else:
        sections = [args.sections.strip()]
    
    # Create scraper instance
    scraper = HSExplanatoryNotesScraper(
        base_url=args.base_url,
        revisions=revisions,
        sections=sections,
        output_dir=args.output_dir,
        headless=args.headless,
        delay=args.delay,
        firefox_binary_path=args.firefox_binary,
        geckodriver_path=args.geckodriver,
        username=args.username,
        password=args.password,
        verbose=args.verbose,
        debug_mode=args.debug
    )
    
    # Initialize driver and perform login
    try:
        scraper.driver = scraper.init_driver()
        
        if args.debug:
            print(f"Debug mode enabled - screenshots will be saved to: {scraper.screenshot_dir}")
        
        # Perform login
        if not scraper.perform_login():
            print("Login failed. Exiting.")
            return 1
        
        print("Login successful!")
        if args.debug:
            print("Debug screenshots saved in temp directory.")
        
        # Run the full scraping process
        print("Starting content scraping...")
        stats = scraper.run_full_scrape()
        
        # Print final statistics
        print("\n" + "="*50)
        print("SCRAPING COMPLETED")
        print("="*50)
        print(f"Sections processed: {stats['sections_processed']}")
        print(f"Chapters processed: {stats['chapters_processed']}")
        print(f"Files created: {stats['files_created']}")
        print(f"Errors encountered: {stats['errors']}")
        print(f"Total duration: {stats.get('duration_seconds', 0):.2f} seconds")
        print(f"Output directory: {scraper.output_dir}")
        print("="*50)
        
        return 0
        
    except Exception as e:
        print(f"Error: {e}")
        return 1
    finally:
        if scraper.driver:
            scraper.driver.quit()


if __name__ == "__main__":
    exit(main())
