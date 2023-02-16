int count = 0;



void setup() {
  // put your setup code here, to run once:
  pinMode(5, OUTPUT);
  Serial.begin(115200);
  
}

void loop() {
  // put your main code here, to run repeatedly:
  count++;
  if(count == 400){
    digitalWrite(5, HIGH);
  } else if (count == 800){
    count = 0;
     digitalWrite(5, LOW);
  }


  Serial.println(analogRead(A0));
  delay(10);
  
}
