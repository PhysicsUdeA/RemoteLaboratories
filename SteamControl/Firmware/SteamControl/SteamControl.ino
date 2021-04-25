/* *****************************************************************************
 * File:     SteamControl.ino                                                  *
 * Project:  RemoteLaboratories                                                *
 * Author:   Fabian Castaño                                                    *
 * Version:  V_1.0                                                             *
 *                                                                             *
 * Created on April 24, 2021, 21:32 PM                                         *
 * *****************************************************************************/

/** ****************************************************************************
 ** ************ INCLUDES ******************************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ DEFINES *******************************************************
 ** ****************************************************************************/
#define TCONTROL 110 // temperatura maxima que puede alcanzar el sistema
#define TDELTA 10    // maxima variacion de temperatura admitida en el control

// tiempo de muestreo
#define TSAMPLE 1000 // tiempo en milisegundos

// pin definition
#define STEAM_PIN 9 // pin para activar el canal 1
#define LASER_PIN 7 // pin para activar el canal 2

//Pines para el protocolo de comunicación con la tarjeta de termocupla.
#define MISOS 6 //Pin 3 conector TEMP.
#define SCKS 5  //Pin 4 conector TEMP.
#define CSS 4   //Pin 10 conector TEMP.

enum
{
    NONE_MENU,
    APAGAR,
    ENCENDER,
    TEPMPORIZADOR,
    TEMPERATURA,
    TIPO_SENSOR
};

// tipo de sensor utilizado, descomente el que desee utilizar
enum
{
    MAX6674_SENSOR,
    MAX6675_SENSOR
};

/** ****************************************************************************
 ** ************ EXTERN VARIABLES **********************************************
 ** ****************************************************************************/

/** ****************************************************************************
 ** ************ VARIABLES *****************************************************
 ** ****************************************************************************/
// contadores de tiempo
long contmsact = 0;
long contmsant = 0;
int contsec = 0;
int contmin = 0;
int contminSet = 15;

int ch1State = 0;
int ch2State = 0;

int tipoSensor = MAX6675_SENSOR;

// banderas para el control de los eventos
bool flvapor = false;
bool flvalid = false;
bool flimprimir = false;

// variable para el control de los menus
int menu = 0;

// variables para el control de la temperatura
double tempReal = 0;
double tempSetp = 110;

// variables para el control de menu
int command = 0;
int value = 0;

bool flProcMenu = false;

/** ****************************************************************************
 ** ************ PROTOTYPES ****************************************************
 ** ****************************************************************************/
// lee_temp6675(); Ejecuta el protocolo necesario para leer el valor de la
// temperatura (12bits con incrementos correspondientes a 0.25°C) con el chip MAX6675
int lee_temp6675(void);

// lee_temp6674(); Ejecuta el protocolo necesario para leer el valor de la
// temperatura (10bits con incrementos correspondientes a 0.125°C) con el chip MAX6674
int lee_temp6674(void);

// funcion para enviar por serial la configuracion de datos obtenida
void sedSerialData(void);

/** ****************************************************************************
 ** ************ CONFIGURATION *************************************************
 ** ****************************************************************************/
void setup()
{
    // configuracion de pines para manejo de termocupla
    pinMode(MISOS, INPUT); // Configures the communication pins for MAX6674 thermocouple chip.
    pinMode(SCKS, OUTPUT);
    pinMode(CSS, OUTPUT);
    digitalWrite(SCKS, LOW); // Inicia el reloj deshabilitado.
    digitalWrite(CSS, HIGH); // Inicia el "Chip Select" en 1.

    // configuracion de los pines de manejo de los canales
    pinMode(STEAM_PIN, OUTPUT); // pines para el control de los canales como salida
    pinMode(LASER_PIN, OUTPUT);
    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(STEAM_PIN, LOW); // desactiva los canales
    digitalWrite(LASER_PIN, LOW);
    digitalWrite(LED_BUILTIN, HIGH);

    // inicializacion del serial
    Serial.begin(115200);

    // reserve 200 bytes for the inputString:
    contmin = contminSet;

    sedSerialData();
    delay(1000);
}

/** ****************************************************************************
 ** ************ LOOP MAIN FUNCTION ********************************************
 ** ****************************************************************************/

void loop()
{
    // control de tiempo
    contmsact = millis();

    if (contmsact - contmsant >= 1000)
    {
        // verifica estado de canal 1
        if (ch1State == 1)
        {
            contsec--;
            if (contsec < 0)
            {
                contsec = 59;
                contmin--;
            }

            if (contmin < 0)
            {
                ch1State = 0;
                digitalWrite(STEAM_PIN, LOW);
                contmin = contminSet;
                contsec = 0;
            }
            if (tempReal > tempSetp)
            {
                ch1State = 0;
                digitalWrite(STEAM_PIN, LOW);
            }
        }

        // verifica el valor de temperatura en el sensor
        if (tipoSensor == MAX6675_SENSOR)
        {
            tempReal = double(lee_temp6675() * 0.25);
        }
        else if (tipoSensor == MAX6674_SENSOR)
        {
            tempReal = double(lee_temp6674() * 0.125);
        }

        // inprime valores por serial
        sedSerialData();
        contmsant = millis();
    }

    // ejecucion de menus
    switch (command)
    {
    case NONE_MENU:
        /* No ejecuta ninguna accion */
        break;

    case APAGAR:
        /* apaga la salida de relay dependiendo del canal */
        switch (value)
        {
        case 1:
            digitalWrite(STEAM_PIN, LOW);
            ch1State = LOW;
            digitalWrite(LED_BUILTIN, LOW);
            break;
        case 2:
            digitalWrite(LASER_PIN, LOW);
            ch2State = LOW;
            digitalWrite(LED_BUILTIN, LOW);
            break;

        default:
            break;
        }
        command = NONE_MENU;
        break;

    case ENCENDER:
        /* enciende la salida de relay dependiendo del canal */
        switch (value)
        {
        case 1:
            digitalWrite(STEAM_PIN, HIGH);
            ch1State = HIGH;
            digitalWrite(LED_BUILTIN, HIGH);
            break;
        case 2:
            digitalWrite(LASER_PIN, HIGH);
            ch2State = HIGH;
            digitalWrite(LED_BUILTIN, HIGH);
            break;

        default:
            break;
        }
        command = NONE_MENU;
        break;

    case TEPMPORIZADOR:
        /* actualiza el valor de la temperatura de control */
        if (value > 0 && value <= 20)
        {
            contminSet = value;
            contsec = 0;
            contmin = contminSet;
        }
        command = NONE_MENU;
        break;

    case TEMPERATURA:
        /* actualiza el valor de la temperatura de control */
        if (value > 20 && value <= 110)
        {
            tempSetp = value;
        }
        command = NONE_MENU;
        break;

    case TIPO_SENSOR:
        /* actualiza el valor de la temperatura de control */
        tipoSensor = value;
        command = NONE_MENU;
        break;

    default:
        break;
    }
}

// funcion para enviar por serial la configuracion de datos obtenida
void sedSerialData(void)
{
    // compone cadena de datos a enviar por serial
    String dataToSend = String(ch1State) + ',' + String(ch2State) + ',' +
                        String(tempSetp) + ',' + String(contminSet) + ',' +
                        String(tempReal) + ',' + String(contmin) + ',' +
                        String(contsec) + ',' + String(tipoSensor) + ';';

    // envia cadena de datos por serial
    Serial.println(dataToSend);
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
    while (Serial.available() > 5)
    {
        // recibe linea de comandos
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
        Serial.flush();
    }
}

/** ****************************************************************************
 ** ************ FUNCTION DEFINITION *******************************************
 ** ****************************************************************************/
// lee_temp6675(); Ejecuta el protocolo necesario para leer el valor de la
// temperatura (12bits con incrementos correspondientes a 0.25°C) con el chip MAX6675
int lee_temp6675(void)
{

    int i = 0;
    int bit_leido;
    int dato_actual;
    int dato_acumulado = 0;
    int temperatura;

    digitalWrite(CSS, 0);
    delayMicroseconds(10);

    for (i = 0; i < 16; i++)
    {
        digitalWrite(SCKS, 1);
        delayMicroseconds(10);               //Se fija SCKS en 50KHz
        bit_leido = digitalRead(MISOS);      //Se va construyendo bit a bit el dato de 16 bits arrojado por el chip MAX6675
        dato_actual = bit_leido << (15 - i); //Se guarda bit_leido en la posición correspondiente de dato_actual.
        dato_acumulado = dato_actual | dato_acumulado;
        digitalWrite(SCKS, 0);
        delayMicroseconds(10);
    }
    digitalWrite(CSS, 1);
    delayMicroseconds(10);
    temperatura = ((dato_acumulado >> 3) & 4095); //La temperatura está en los bits 14->3 de dato_acumulado. Temperatura es una variable entre 0 y 4095, que representa incrementos de 0.25°C
    return temperatura;
}

// lee_temp6674(); Ejecuta el protocolo necesario para leer el valor de la
// temperatura (10bits con incrementos correspondientes a 0.125°C) con el chip MAX6674
int lee_temp6674()
{

    int i = 0;
    int bit_leido;
    int dato_actual;
    int dato_acumulado = 0;
    int temperatura;

    digitalWrite(CSS, 0);
    delayMicroseconds(10);

    for (i = 0; i < 16; i++)
    {
        digitalWrite(SCKS, 1);
        delayMicroseconds(10);               //Se fija SCK en 50KHz
        bit_leido = digitalRead(MISOS);      //Se va construyendo bit a bit el dato de 16 bits arrojado por el chip MAX6674
        dato_actual = bit_leido << (15 - i); //Se guarda bit_leido en la posición correspondiente de dato_actual.
        dato_acumulado = dato_actual | dato_acumulado;
        digitalWrite(SCKS, 0);
        delayMicroseconds(10);
    }
    digitalWrite(CSS, 1);
    delayMicroseconds(10);
    temperatura = ((dato_acumulado >> 5) & 1023); //La temperatura está en los bits 14->5 de dato_acumulado. Temperatura es una variable entre 0 y 1023, que representa incrementos de 0.125°C
    return temperatura;
}

/********************** (C) COPYRIGHT GIBIC MedTech ****** END OF FILE ****/
