package migration;

import org.junit.Test;

public class testMigration {

	@Test
	public void testBase64() throws Exception {
		byte[] b = "chaine complète".getBytes();
		System.out.println("Java = " + java.util.Base64.getEncoder().encodeToString(b));
		System.out.println("GCry = " + Base64.encode(b));
	}
	
}
