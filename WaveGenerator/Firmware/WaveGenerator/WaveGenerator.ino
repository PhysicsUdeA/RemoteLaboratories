/* *****************************************************************************
 * File:     WaveGenerator.ino                                                 *
 * Project:  RemoteLaboratories                                                *
 * Author:   Fabian Castaño                                                    *
 * Version:  V_1.0                                                             *
 *                                                                             *
 * Created on April 30, 2021, 16:39 PM                                         *
 * *****************************************************************************/
// Library availabe on: https://github.com/Billwilliams1952/AD9833-Library-Arduino

/** ****************************************************************************
 ** ************ INCLUDES ******************************************************
 ** ****************************************************************************/
#include "AD9833.h"

/** ****************************************************************************
 ** ************ DEFINES *******************************************************
 ** ****************************************************************************/
#define LED_PIN 13 // I'm alive blinker
#define FNC_PIN 4  // Any digital pin. Used to enable SPI transfers (active LOW)

// Some macros to 'improve' readability
#define BLINK_LED digitalWrite(LED_PIN, millis() % 1000 > 500);

enum
{
    NONE_MENU,
    APAGAR,
    ENCENDER,
    FRECUENCIA
};

/** ****************************************************************************
 ** ************ EXTERN VARIABLES **********************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ VARIABLES *****************************************************
 ** ****************************************************************************/
// variables para el control de menu
int command = 0;
float value = 0;

static bool outputOn = false; // variable que controla el encendido o apagado del modulo

//--------------- Create an AD9833 object ----------------
// Note, SCK and MOSI must be connected to CLK and DAT pins on the AD9833 for SPI
// -----      AD9833 ( FNCpin, referenceFrequency = 25000000UL )
AD9833 gen(FNC_PIN); // Defaults to 25MHz internal reference frequency

/** ****************************************************************************
 ** ************ PROTOTYPES ****************************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ CONFIGURATION *************************************************
 ** ****************************************************************************/

void setup()
{
    pinMode(LED_PIN, OUTPUT);

    while (!Serial); // Delay until terminal opens
    Serial.begin(115200);

    // This MUST be the first command after declaring the AD9833 object
    gen.Begin();             // The loaded defaults are 1000 Hz SINE_WAVE using REG0
                             // The output is OFF, Sleep mode is disabled
    gen.EnableOutput(false); // Turn ON the output

    delay(500);
    Serial.print("0,0;");
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
        BLINK_LED;
        break;

    case APAGAR:
        /* apaga la salida de ondas en el generador */
        outputOn = false;
        gen.EnableOutput(outputOn); // Turn off output
        Serial.print("1,0;");

        command = NONE_MENU;
        break;

    case ENCENDER:
        /* enciende la salida de ondas en el generador */
        outputOn = true;
        gen.EnableOutput(outputOn); // Turn on output
        Serial.print("2,0;");

        command = NONE_MENU;
        break;

    case FRECUENCIA:
        /* actualiza el valor de la temperatura de control */
        if (value > 0 && value <= 100000)
        {
            gen.ApplySignal(SQUARE_WAVE, REG0, value);
            String data = "3," + String(gen.GetActualProgrammedFrequency(REG0), 1) + ";";
            Serial.print(data);
        }
        command = NONE_MENU;
        break;

    default:
        break;
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
        value = dataIn2[1].toFloat();
        Serial.flush();
    }
}

/********************** (C) COPYRIGHT GIBIC MedTech ****** END OF FILE ****/
