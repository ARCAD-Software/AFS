package test.rcp;

import com.arcadsoftware.afs.client.core.connection.ITrustStoreProvider;

public class TrustStoreProvider implements ITrustStoreProvider {

	public TrustStoreProvider() {
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public String getTrustStorePath() {
		return "/home/marc/tests/server/security/truststore.ip.jks";
	}

	@Override
	public char[] getTrustStorePassword() {
		return "quadra".toCharArray();
	}

	@Override
	public String getKeyStorePath() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public char[] getKeyStorePassword() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setTrustStorePath(String path) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTrustStorePassword(char[] pwd) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setKeyStorePath(String path) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setKeyStorePassword(char[] pwd) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void resetToDefault() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean save() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public char[] getKeyPassword() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setKeyPassword(char[] password) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getKeyStoreType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setKeyStoreType(String type) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getTrustStoreType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setTrustStoreType(String type) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getKeyManagerAlgorithm() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setKeyManagerAlgorithm(String keyAlgorithm) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getTrustManagerAlgorithm() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setTrustManagerAlgorithm(String trustAlgorithm) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getDisabledCipherSuites() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setDisabledCipherSuites(String disabledCiphers) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getDisabledProtocols() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setDisabledProtocols(String disabledProtocols) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getEnabledCipherSuites() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setEnabledCipherSuites(String enabledCiphers) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getEnabledProtocols() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setEnabledProtocols(String enabledProtocols) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getProtocol() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setProtocol(String protocol) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getSecureRandomAlgorithm() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setSecureRandomAlgorithm(String randomAlgorithm) {
		// TODO Auto-generated method stub
		
	}

}
