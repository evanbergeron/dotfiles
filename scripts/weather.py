import requests
import xml.etree.ElementTree as ET

url = 'http://wsf.cdyne.com/WeatherWS/Weather.asmx?op=GetCityWeatherByZIP'
url2 = 'http://wsf.cdyne.com/WeatherWS/Weather.asmx/GetCityWeatherByZIP'
r = requests.get(url2, params={'ZIP':'15213'})

tree = ET.fromstring(r.text)

root = {}

for child in tree:
    root[child.tag.split("}")[1]] = child.text

print "%s: %s, %s" % (root["City"], root['Temperature'], root['Description'])
