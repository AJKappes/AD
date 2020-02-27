# Python file for nutrient waste recycling model

### nwr model ###

# phosphorus #

def phos_func(*args, loc, soil_test, acres):
    '''
    Function takes feedstock and manure volume arguments and computes phosphorus nutrient outflow
    in kg/time step.

    3.79 constant converts gallon to liter (1L = 1kg)
    0.9112 nutrient portion remaining post liquid-solid separation after AD output to lagoon
    2.205 constant converts kilogram to pound
    0.00378541 constant converts mg/ml to kg/gal
    1000 constant converts kg to cubic meter
    kWh 1.02264 correction factor, 39.2 calorific value, 3.6 conversion factor
    '''
    coefs = [.035, .107, .34, .011, .13, .023]
    # [blood, fish_by, paper_pulp, ob, gt, manure]
    bio_coefs = [0.73, 0.475, 0.59, 1.45, 1.9, 0.75]
    tip_fees = [0.03609, 0.05, 0.0415, 0.05, 0.05, 0]
    out_list = []
    rev_list = []
    biogas_list = []

    for i in range(len(coefs)):
        out_list.append(3.79 * 2.205 * coefs[i] * args[i]) # convert gals to liters to kg to lbs
        rev_list.append(tip_fees[i] * args[i])
        # convert mg/ml biogas potential to kg/gal for biogas kg output in cubic meters
        biogas_list.append(0.00378541 * 1000 * bio_coefs[i] * args[i])

    phos_out = round(0.9112 * sum(out_list), 3) # proportion remaining post liquid-solid sep
    lb_acre = round(phos_out / acres, 3)
    input_rev = round(sum(rev_list), 3)
    biogas_pot = round(sum(biogas_list), 3)
    kWh = round(1.02264 * 39.2 * biogas_pot / 3.6)

    if loc == 'West' or loc == 'west':
        if soil_test < 20:
            p_index = 'Low'
            rec_p_app = '0-300'
            if lb_acre > 300:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if 20 <= soil_test < 40:
            p_index = 'Medium'
            rec_p_app = '0-200'
            if lb_acre > 200:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if 40 <= soil_test <= 100:
            p_index = 'High'
            rec_p_app = '0-30'
            if lb_acre > 30:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if soil_test > 100:
            p_index = 'Excessive'
            rec_p_app = '0'
            app_decision = 'Do not apply'

    if loc == 'East' or loc == 'east':
        if soil_test < 10:
            p_index = 'Low'
            rec_p_app = '0-300'
            if lb_acre > 300:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if 10 <= soil_test < 25:
            p_index = 'Medium'
            rec_p_app = '0-200'
            if lb_acre > 200:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if 25 <= soil_test <= 50:
            p_index = 'High'
            rec_p_app = '0-30'
            if lb_acre > 30:
                app_decision = 'Do not apply'
            else:
                app_decision = 'Apply'

        if soil_test > 50:
            p_index = 'Excessive'
            rec_p_app = '0'
            app_decision = 'Do not apply'

    return [phos_out, lb_acre, p_index, rec_p_app, app_decision, input_rev, kWh]


#phos_func(10, 15, 20, 25, 30, 35, loc='west', soil_test=51, acres=4)

