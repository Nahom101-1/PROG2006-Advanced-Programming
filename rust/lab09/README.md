# Lab 09: REST APIs

## Overview

This lab involves building a simple REST API using **Rust** or **Haskell**. The goal is to implement a web server that handles HTTP requests, processes JSON data, and serves both JSON and HTML responses. The lab is divided into two main parts: a **Hello JSON API** and a **Course Announcements API**. NOTE you will need to add own github token to run this. 

---

## Objectives

1. Build a web server using **Rust** (or **Haskell**).
2. Implement endpoints for handling HTTP GET and POST requests.
3. Serve JSON and HTML responses based on the request.
4. Ensure proper error handling for invalid requests or server errors.

---

## Part 1: Hello JSON API

### Endpoints

1. **GET /hello**  
   - Returns: `"Hello, World!"`  
   - Status: `200 OK`

2. **GET /**  
   - Returns: `404 Page Not Found`  
   - Status: `404 Not Found`

3. **GET /greet/<name>**  
   - Example: `GET /greet/Mariusz`  
   - Returns:  
     ```json
     {
       "greet": "Hello",
       "name": "Mariusz"
     }
     ```  
   - Status: `200 OK`

4. **POST /greetme**  
   - Input:  
     ```json
     {
       "input": "whatever text",
       "name": "Mariusz"
     }
     ```  
   - Returns:  
     ```json
     {
       "msg": "whatever text Mariusz"
     }
     ```  
   - Status: `200 OK`

---

## Part 2: Course Announcements API

### Functionality

1. **GET /announcements**  
   - Serves a list of course announcements in either **HTML** or **JSON** format.
   - Query Parameter:  
     - `?format=json` for JSON response.
     - `?format=html` for HTML response.

2. **Data Source**  
   - Fetch announcements from the GitLab API:  
     ```
     https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened
     ```
   - Sort announcements from earliest to latest.
   - Include the following details:
     - Date
     - Title
     - Content



## How to Run


1. Install dependencies:
   ```bash
   cargo build
   ```

2. Run the server:
   ```bash
   cargo run
   ```

3. Access the endpoints:
   - 

http://localhost:3000/hello


   - 

http://localhost:3000/greet/<name>


   - 

http://localhost:3000/greetme


   - 

http://localhost:3000/announcements



