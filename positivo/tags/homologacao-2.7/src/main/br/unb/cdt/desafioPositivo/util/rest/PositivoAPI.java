package br.unb.cdt.desafioPositivo.util.rest;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;

import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.seam.core.SeamResourceBundle;

/**
 * Classe abstrata que pode ser reusada para implementar 
 * requisicoes aos servicos da Positivo.
 * 
 * @author rbonifacio
 */
public abstract class PositivoAPI {

	protected ClientRequest req;
	
	//TODO: esse identificador deve vir de um arquivo de configuracao!!!!
	protected static final String ID_APLICACAO = "s5+omQY9f8qcvUy7Rtbh3oY/2sc=";
	protected java.util.ResourceBundle bundle = SeamResourceBundle.getBundle("webservices");
	
	//protected abstract String parametros();
	protected abstract void atualizaParametros();
	protected abstract String url();

	/**
	 * Um <i>template method</i> que prepara todas as requisicoes 
	 * a API da positivo. 
	 */
	public void preparaRequisicao() {
		req = new ClientRequest(url());
		req.accept("application/json");
		req.queryParameter("idAplicacao", ID_APLICACAO);
		atualizaParametros();
	}
	
	/**
	 * Realiza uma requisicao a API da positivo.
	 * @return Retorna o resultado da requisicao no formato JSON.
	 */
	public RespostaPositivo requisitaServico() throws Exception {
		ClientResponse<String> resp = req.post(String.class);
    	
    	BufferedReader br = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(resp.getEntity().getBytes())));
   
    	String output;
    	StringBuffer buffer = new StringBuffer();
		
    	while ((output = br.readLine()) != null) {
			buffer.append(output);
		}
		
    	return (new RespostaPositivo()).fromJASON(buffer.toString());
	}
	
}
