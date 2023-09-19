#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 15 18:28:23 2023

@author: mutua
"""


##Calculation of areaHA
import json

def areaHA_function(event, context):
    # Parse the input JSON
    try:
        input_data = json.loads(event['body'])
        areaUnits = input_data.get('areaUnits', '')
        area = input_data.get('area', 0)
    except Exception as e:
        return {
            'statusCode': 400,
            'body': json.dumps({'error': 'Invalid input JSON'}),
        }

    # Perform the area calculation
    if areaUnits == "ha":
        areaHa = area
    elif areaUnits == "acre":
        areaHa = area / 2.47105
    else:
        areaHa = area / 10000

    # Return the result as JSON
    response = {
        'areaHa': areaHa
    }

    return {
        'statusCode': 200,
        'body': json.dumps(response),
    }



###arn:aws:iam::574205726572:role/lambda-apigateway-role