import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

public class UtilsRSA {

	public static void main(String[] args) throws NoSuchAlgorithmException {

		KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA");

		kpg.initialize(2048);

		KeyPair kp = kpg.generateKeyPair();

		System.out.print("byte[] DECRYPT = new byte[] {");
		byte[] key = kp.getPublic().getEncoded();
		boolean first = true;
		for (byte b: key) {
			if (first) {
				first = false;
			} else {
				System.out.print(", ");				
			}
			System.out.print(b);
		}
		System.out.println("};");
		System.out.println("");
		System.out.print("byte[] ENCRYPT = new byte[] {");
		key = kp.getPrivate().getEncoded();
		first = true;
		for (byte b: key) {
			if (first) {
				first = false;
			} else {
				System.out.print(", ");				
			}
			System.out.print(b);
		}
		System.out.println("};");
		System.out.println("");
	}

}
