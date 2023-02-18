/* *****************************************************************************
 * File:     Capacitor.ino                                                     *
 * Project:  RemoteLaboratories                                                *
 * Author:   Fabian Castaño                                                    *
 * Version:  V_1.0                                                             *
 *                                                                             *
 * Created on February 17, 2023, 16:39 PM                                      *
 * *****************************************************************************/

/** ****************************************************************************
 ** ************ INCLUDES ******************************************************
 ** ****************************************************************************/
#include <Arduino.h>
#include "timer.h"

/** ****************************************************************************
 ** ************ DEFINES *******************************************************
 ** ****************************************************************************/
#define LED_PIN 13 // I'm alive blinker
#define CAP_DIG_1 3
#define CAP_DIG_2 4
#define CAP_DIG_3 5
#define CAP_ANA_1 A0
#define CAP_ANA_2 A1
#define CAP_ANA_3 A2

// Some macros to 'improve' readability
#define BLINK_LED digitalWrite(LED_PIN, millis() % 1000 > 500);

enum
{
    NONE_MENU,
    CAPACITOR_1_ON,
    CAPACITOR_2_ON,
    CAPACITOR_3_ON,
    CAPACITOR_1,
    CAPACITOR_2,
    CAPACITOR_3,
};

/** ****************************************************************************
 ** ************ EXTERN VARIABLES **********************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ VARIABLES *****************************************************
 ** ****************************************************************************/
// variables para el control de menu
int command = 0;
int value = 0;

int contSample = 0;
int contPeriod = 0;
int selectedADC = CAP_ANA_1;
int selectedToggle = CAP_DIG_1;
int adcValue = 0;

int pinState = LOW;

volatile uint8_t flagPrint = false;

int samplePeriod = 10; // 10ms

/** ****************************************************************************
 ** ************ PROTOTYPES ****************************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ CONFIGURATION *************************************************
 ** ****************************************************************************/

void setup()
{
    pinMode(LED_PIN, OUTPUT);
    pinMode(CAP_DIG_1, OUTPUT);
    pinMode(CAP_DIG_2, OUTPUT);
    pinMode(CAP_DIG_3, OUTPUT);

    // Timer initialization
    init_Timer();

    // Serial initialization
    Serial.begin(115200);

    //  Serial.print("0,0;");
}

/** ****************************************************************************
 ** ************ LOOP MAIN FUNCTION ********************************************
 ** ****************************************************************************/

void loop()
{
    BLINK_LED
    // ejecucion de menus
    switch (command)
    {
    case NONE_MENU:
        /* No ejecuta ninguna accion */

        break;

    case CAPACITOR_1_ON:
        /* apaga la salida de ondas en el generador */

        selectedADC = CAP_ANA_1;
        selectedToggle = CAP_DIG_1;

        pinState = LOW;
        contPeriod = 0;
        digitalWrite(CAP_DIG_1, pinState);
        digitalWrite(CAP_DIG_2, pinState);
        digitalWrite(CAP_DIG_3, pinState);

        command = CAPACITOR_1;
        break;

    case CAPACITOR_2_ON:
        /* apaga la salida de ondas en el generador */

        selectedADC = CAP_ANA_2;
        selectedToggle = CAP_DIG_2;

        pinState = LOW;
        contPeriod = 0;
        digitalWrite(CAP_DIG_1, pinState);
        digitalWrite(CAP_DIG_2, pinState);
        digitalWrite(CAP_DIG_3, pinState);

        command = CAPACITOR_2;
        break;

    case CAPACITOR_3_ON:
        /* apaga la salida de ondas en el generador */

        selectedADC = CAP_ANA_3;
        selectedToggle = CAP_DIG_3;

        pinState = LOW;
        contPeriod = 0;
        digitalWrite(CAP_DIG_1, pinState);
        digitalWrite(CAP_DIG_2, pinState);
        digitalWrite(CAP_DIG_3, pinState);

        command = CAPACITOR_3;
        break;

    default:
        break;
    }

    if (flagPrint == true && command != NONE_MENU)
    {
        flagPrint = false;
        // print the ADC Value
        adcValue = analogRead(selectedADC);
        Serial.println(adcValue);
    }
}

/** ****************************************************************************
 ** ************ INTERRUPTION HANDLER ******************************************
 ** ****************************************************************************/
/*
  SerialEvent occurs whenever a new data comes in the hardware serial RX. This
  routine is run between each time loop() runs, so using delay inside loop can
  delay response. Multiple bytes of data may be available.
*/
void serialEvent()
{
    // lectura de datos en el puerto serial
    while (Serial.available() > 3)
    {
        // recibe linea de comandos
        // Comando = 1,100000.0;
        String dataIn = Serial.readStringUntil(';');

        // procesamiento de comando
        int contComas = 0;
        for (int i = 0; i < (int)dataIn.length(); i++)
        {
            if (dataIn[i] == ',')
            {
                contComas++;
            }
        }

        String dataIn2[4];
        for (int i = 0; i < contComas + 1; i++)
        {
            dataIn2[i] = dataIn.substring(0, dataIn.indexOf(','));
            dataIn = dataIn.substring(dataIn.indexOf(',') + 1);
        }

        // almacena los datos de comando
        command = dataIn2[0].toInt();
        value = dataIn2[1].toInt();
        samplePeriod = dataIn2[2].toInt();
        Serial.flush();
    }
}

/* ****************************************************************
 * ********** TIMER INTERRUPT *************************************
 * ****************************************************************/
ISR(TIMER1_COMPA_vect)
{ // timer1 interrupt 1Hz toggles pin 13 (LED)
  // generates pulse wave of frequency 1Hz/2 = 0.5kHz (takes two cycles for full wave- toggle high then toggle low)

    contSample++;
    if (command == CAPACITOR_1 || command == CAPACITOR_2 || command == CAPACITOR_3)
    {
        contPeriod++;

        // toggle pin
        if (contPeriod >= value)
        {
            contPeriod = 0;
            if (pinState == LOW)
            {
                pinState = HIGH;
            }
            else
            {
                pinState = LOW;
            }

            digitalWrite(selectedToggle, pinState);
        }
    }
    else
    {
        contPeriod = 0;
        pinState = LOW;
    }

    // activate sample
    if (contSample >= samplePeriod) // 10ms
    {
        contSample = 0;
        flagPrint = true;
    }
}

/********************** (C) COPYRIGHT GLIA Tech ****** END OF FILE ****/
