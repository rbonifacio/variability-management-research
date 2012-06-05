package br.unb.cdt.desafioPositivo.model.acesso;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.DiscriminatorType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import br.unb.cdt.desafioPositivo.model.Usuario;

@Entity
@Inheritance(strategy=InheritanceType.JOINED)
@Table(name="TB_ACESSO_USUARIO")
@DiscriminatorColumn(name="estadoAcesso",discriminatorType=DiscriminatorType.INTEGER)
public abstract class AcessoUsuario implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	private Long id;
	
	@Column(name="DATA_INICIO")
	protected Date dataInicio;
	
	@Column(name="DATA_FIM")
	protected Date dataFim;
	
	@ManyToOne(cascade=CascadeType.ALL)
	protected Usuario usuario;
	
	/**
	 * Construtor padrao para o acesso dos usuario. 
	 * Note que, conforme essa implementacao, quando criado um 
	 * novo acesso do usuario, ele tem um status de atual.
	 */
	public AcessoUsuario() {
		dataInicio = Calendar.getInstance().getTime();
	}
	
	/**
	 * Metodo usado para notificar o resultado de uma autenticacao do 
	 * usuario. Essa implementacao eh um pouco curiosa, pois apenas informamos 
	 * se o usuario ja foi autenticado ou nao (apos o resultado de uma interacao 
	 * com o servico correspondente da positivo). 
	 * 
	 * @param autenticacao indica se o usuario foi autenticado ou nao.
	 */
	public abstract void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario;
	
	/**
	 * Notifica o interesse real do cadastro como proponente do sistema de 
	 * submissao de ideias. 
	 * 
	 * @param token Token informado pela positivo. 
	 */
	public abstract void confirmarCadastro(String token) throws ExcecaoAcessoUsuario; 
	
	/**
	 * Realiza o desbloqueio do acesso de um proponente.
	 */
	public abstract void desbloquear() throws ExcecaoAcessoUsuario;
	
	/**
	 * Realiza a alteracao de senha do usuario. 
	 * 
	 * @throws ExcecaoAcessoUsuario
	 */
	public abstract void alterarSenha() throws ExcecaoAcessoUsuario;

	public Usuario getUsuario() {
		return usuario;
	}

	public void setUsuario(Usuario usuario) {
		this.usuario = usuario;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Date getDataInicio() {
		return dataInicio;
	}

	public void setDataInicio(Date dataInicio) {
		this.dataInicio = dataInicio;
	}

	public Date getDataFim() {
		return dataFim;
	}

	public void setDataFim(Date dataFim) {
		this.dataFim = dataFim;
	}
}
