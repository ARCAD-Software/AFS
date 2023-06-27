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
	public String getTrustStorePassword() {
		return "quadra";
	}

	@Override
	public String getKeyStorePath() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getKeyStorePassword() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setTrustStorePath(String path) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTrustStorePassword(String path) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setKeyStorePath(String path) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setKeyStorePassword(String path) {
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

}
