import java.util.List;

import org.json.JSONObject; // This is part of the Android API
import org.json.JSONArray; // This is part of the Android API

class HERMITClientTest {
        
        
        public static void main(String[] args) {
                System.out.println("Starting test");
                
                HERMITClient client = HERMITClient.connect(new HERMITClient.Connection() {
                        public String post(String url, String arg) throws java.io.IOException {
                                System.out.println("Calling post: " + url);
                                System.out.println("Sending: " + arg);
                                String result = "";
                                if (url.equals("/connect")) {
                                        result = "{'unique':99,'token':22}";
                                } else {
                                        throw new java.io.IOException("bad url" + url);
                                }
                                System.out.println("Replying: " + result);
                                return result;
                        }
                });

                System.out.println("created client");

        }

}
