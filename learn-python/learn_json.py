import json


cookies = [
    {"flavor": "Chocolate Chip", "calories": 180},
    {"flavor": "Snicker Doodle", "calories": 220},
    {"flavor": "Oatmeal Raisin", "calories": 120},
]

print(json.dumps(cookies, indent=2))
